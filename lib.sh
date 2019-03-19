TEMPLATES=$ROOT/templates
FILES=$ROOT/files

diff() {
	if command -v git >/dev/null; then
		git diff --no-index "$@"
	else
		comamnd diff -u "$@"
	fi
}

pkg() {
	for p in $*; do
		pacman -Q $p >/dev/null 2>&1 || pacman -S $p
	done
}

_f() {
	local dst=$1
	local src=$2
	mkdir -p $(dirname $dst)
	if [ -e $dst ]; then
		diff $dst $src || :
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
		shift
	else
		local src=$TEMPLATES$2
		shift 2
	fi

	local tmp=/tmp/$(echo $dst | sed 's#/#_#g')
	envsubst "$@" < $src > $tmp
	_f $dst $tmp
}

svc() {
	for a in enable start; do
		systemctl $a $1
	done
}
