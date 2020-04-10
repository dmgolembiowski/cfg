ROOT=$(cd "$(dirname "$0")"; pwd -P)

PROMETHEUS_V=2.17.1
ALERTMANAGER_V=0.20.0
NODEEXPORTER_V=0.18.1
PSMEM_V=3.13
AZURECLI_V=2.1.0
MINIFLUX_V=0.0.11
THELOUNGE_V=4.1.0

if [ -e $ROOT/env/ver.sh ]; then
	. $ROOT/env/ver.sh
fi
