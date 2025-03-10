{

  codchi.initScript = ''
    echo This is some extra text which is shown on machine init.
    echo Please run this special command to get started.

    echo Waiting some time...
    sleep 10

    echo My secret: CODCHI_TEST=$CODCHI_TEST
  '';

  codchi.welcome.extraText = ''
    Hihi this issues a deprecation warning
  '';

  codchi.secrets.env.TEST.description = ''
    This is a example secret.
  '';

  systemd.services.my-secret-service = {
    description = "My Service which reads codchi's secrets";
    wantedBy = [ "multi-user.target" ];

    script = ''
      source /etc/codchi-env
      echo "Got secret CODCHI_TEST=$CODCHI_TEST from codchi"
    '';
  };

}
