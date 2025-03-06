{ consts, ... }: {

  _module.args.consts = {
    store = rec {
      DIR_CONFIG = "/config";
      DIR_CONFIG_STORE = "${consts.store.DIR_CONFIG}/store";
      PROFILE_STORE = "${consts.store.DIR_CONFIG_STORE}/profile";
      DIR_CONFIG_MACHINE = "${consts.store.DIR_CONFIG}/machine/$CODCHI_MACHINE_NAME";

      DIR_DATA = "/data";
      DIR_DATA_MACHINE = "${consts.store.DIR_DATA}/machine/$CODCHI_MACHINE_NAME";

      DIR_LOG = "${consts.store.DIR_DATA}/log";
      LOGFILE = "${consts.store.DIR_LOG}/store.log";
      MACHINE_LOG = "${consts.store.DIR_LOG}/machine-$CODCHI_MACHINE_NAME.log";

      # WSL tricks
      DIR_MACHINE_DATA = "/machine-data";
      DIR_MACHINE_DATA_MACHINE = "${consts.store.DIR_MACHINE_DATA}/machine/$CODCHI_MACHINE_NAME";

      NETNS_SUBNET_BASE = "10.6.3";
      NETNS_BRIDGE_ADDR = "${NETNS_SUBNET_BASE}.1";
    };
    machine = {
      USER = "codchi";
      INIT_ENV = "/etc/codchi-env";
    };
    
  };


}
