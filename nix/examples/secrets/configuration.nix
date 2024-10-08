{

  codchi.welcome.extraText = ''
    This is some extra text which is shown on shell login.
    echo Please run this special command to get started.
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
