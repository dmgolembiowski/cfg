TEMPLATES=$ROOT/templates
FILES=$ROOT/files

DISTRO=$(awk -F= '/^ID=/ { print $2 }' /etc/os-release)
ROLES=

_ROLE_PY="
import sys
import yaml

data = yaml.safe_load(open('$ROOT/env.yml', 'r'))

sys.stdout.write(':'.join(data['roles']))
"

role() {
	if [ -z "$ROLES" ]; then
		ROLES=$(python3 -c "$_ROLE_PY")
	fi

	case "$ROLES" in
		$1|$1:*|*:$1|*:$1:*)
			return 0
			;;
	esac

	return 1
}

distro() {
	[ "$DISTRO" = "$1" ]
}

diff() {
	if command -v git >/dev/null; then
		git --no-pager diff --no-index "$@"
	else
		comamnd diff -u "$@"
	fi
}

pkg() {
	for p in $*; do
		case $DISTRO in
			arch)
				pacman -Q $p >/dev/null 2>&1 ||
					pacman -S $p
				;;
			alpine)
				grep -q "^$p\$" /etc/apk/world ||
					apk add $p
				;;
		esac
	done
}

_f() {
	local dst=$1
	local src=$2

	[ -d $(dirname $dst) ] || mkdir -p $(dirname $dst)

	if cmp -s $src $dst; then
		return
	fi

	if [ -e $dst ]; then
		diff $dst $src || :
	else
		diff /dev/null $src || :
	fi
	cp $src $dst
}

file() {
	_f $1 $FILES$1
}

tmpl() {
	local dst=$1

	if [ -e $TEMPLATES$dst ]; then
		local src=$TEMPLATES$dst
	else
		local src=$TEMPLATES$2
	fi

	local tmp=/tmp/$(echo $dst | sed 's#/#_#g')

	python3 - $src <<-EOF > $tmp
	import sys
	import yaml
	import jinja2

	tmpl = jinja2.Template(open(sys.argv[1]).read(), trim_blocks=True, lstrip_blocks=True, keep_trailing_newline=True)
	data = yaml.safe_load(open('$ROOT/env.yml', 'r'))

	sys.stdout.write(tmpl.render(data))
	EOF

	_f $dst $tmp
}

svc() {
	local s=$1
	shift

	case $DISTRO in
		arch)
			local a
			for a in enable start; do
				systemctl "$@" $a $s
			done
			;;
		alpine)
			if ! /etc/init.d/$s -q status >/dev/null; then
				/etc/init.d/$s start
			fi
			if [ ! -e /etc/runlevels/default/$s ]; then
				rc-update add $s
			fi
			;;
	esac
}
