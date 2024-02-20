{ consts, ... }: {

  _module.args.consts = {
    store = {
      DIR_CONFIG = "/config";
      DIR_CONFIG_STORE = "${consts.store.DIR_CONFIG}/store";
      PROFILE_STORE = "${consts.store.DIR_CONFIG_STORE}/profile";
      DIR_CONFIG_MACHINE = "${consts.store.DIR_CONFIG}/machine/$CODCHI_MACHINE_NAME";

      DIR_DATA = "/data";
      DIR_DATA_MACHINE = "${consts.store.DIR_DATA}/machine/$CODCHI_MACHINE_NAME";
    };
    machine = {
      USER = "codchi";
    };
  };


}
