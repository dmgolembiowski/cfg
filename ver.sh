ROOT=$(cd "$(dirname "$0")"; pwd -P)

PROMETHEUS_V=2.13.0
ALERTMANAGER_V=0.19.0
NODE_EXPORTER_V=0.18.1

if [ -e $ROOT/env/ver.sh ]; then
	. $ROOT/env/ver.sh
fi
