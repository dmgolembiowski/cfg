ROOT=$(cd "$(dirname "$0")"; pwd -P)

PROMETHEUS_V=2.13.0
ALERTMANAGER_V=0.19.0
NODE_EXPORTER_V=0.18.1
PSMEM_V=3.13
AZURECLI_V=2.1.0
MINIFLUX_V=0.0.10

if [ -e $ROOT/env/ver.sh ]; then
	. $ROOT/env/ver.sh
fi
