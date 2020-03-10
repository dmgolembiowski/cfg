TEMPLATES=$ROOT/templates
FILES=$ROOT/files

# Read YAML file into shell vars
eval $(python3 -c "
import re
import yaml

def p(n, d):
	for k, v in d.items():
		if isinstance(v, str):
			s = '_'.join(n + [k]).upper()
			if re.match(r'^\w+\$', s):
				print('{}=\"{}\"'.format(s, v))
		elif isinstance(v, dict):
			n.append(k)
			p(n, v)
		elif isinstance(v, list):
			for i in v:
				if isinstance(i, str) and re.match(r'^\w+\$', i):
					s = '_'.join(n + [k, i]).upper()
					print('{}=\"{}\"'.format(s, 'yes'))
	if len(n):
		n.pop()

p([], yaml.safe_load(open('$ROOT/env/$(hostname).yml', 'r')))

")

role() {
	local n=ROLES_$(echo $1 | tr '[a-z]' '[A-Z]')
	eval [ \"\$$n\" = yes ]
}

diff() {
	if command -v git >/dev/null; then
		git --no-pager diff --no-index "$@"
	else
		command diff -u "$@"
	fi
}

_pkg_installed() {
	dpkg-query -Wf '${Package}\n' | grep -q "^$1\$"
}

pkg() {
	for p in $*; do
		if _pkg_installed $p; then
			continue
		fi

		apt install $p
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

envfile() {
	_f $1 $ROOT/env/files/$1
}

tmpl() {
	local dst=$1
	local src=$2
	local k=$3

	if [ -e $TEMPLATES$dst ]; then
		local src=$TEMPLATES$dst
	else
		local src=$TEMPLATES$src
	fi

	local tmp=/tmp/$(echo $dst | sed 's#/#_#g')

	cat $src | _tmpl $k > $tmp
	_f $dst $tmp
	rm $tmp
}

_tmpl() {
	cat | python3 -c "
import sys
import yaml
import jinja2

k = '$1'

tmpl = jinja2.Template(
	sys.stdin.read(),
	trim_blocks=True,
	lstrip_blocks=True,
	keep_trailing_newline=True
)
data = yaml.safe_load(open('$ROOT/env/$(hostname).yml', 'r'))

if len(k):
		lookup = ''.join(['[\"' + i + '\"]' for i in k.split('.')])
		data = eval('data' + lookup)
		data['_key'] = k.split('.')[-1]

sys.stdout.write(tmpl.render(data))
"
}

tmplexec() {
	cat | _tmpl > /tmp/tmplexec

	. /tmp/tmplexec
	rm /tmp/tmplexec
}

svc() {
	local s=$1
	shift

	local a
	systemctl is-enabled "$@" $s >/dev/null || systemctl enable "$@" $s
	case "$s" in
		*.socket)
			:
			;;
		*)
			systemctl start "$@" $s
			;;
	esac
}

pip() {
	local tmpcache
	local venv=$1
	local namever=$2

	if ! $venv/bin/pip freeze | grep -q "^$namever"; then
		tmpcache=$(mktemp -d)
		$venv/bin/pip --cache-dir=$tmpcache install $namever
	fi
}
