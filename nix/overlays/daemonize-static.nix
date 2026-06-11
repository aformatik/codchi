final: prev: {
  daemonize = prev.daemonize.overrideAttrs (old: {
    # Its old Autoconf probe cannot execute the target binary while cross-building.
    configureFlags = (old.configureFlags or [ ])
      ++ final.lib.optionals
      (final.stdenv.buildPlatform != final.stdenv.hostPlatform)
      [
        "ac_cv_func_setpgrp_void=${final.lib.boolToYesNo (!final.stdenv.hostPlatform.isBSD)}"
      ];
  });
}
