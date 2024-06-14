{ lib, ... }:
let
  inherit (lib) types mkOption literalExpression;
in
{
  options.codchi.secrets.env = mkOption {
    type = types.attrsOf (types.submodule ({ name, ... }: {
      options = {
        name = mkOption {
          default = name;
          type = types.strMatching "^[a-zA-Z0-9:_\\.-]*$";
          description = ''
            Name of this environment variable. Will be prefixed by `CODCHI_`.
          '';
        };
        description = mkOption {
          type = types.lines;
          description = ''
            A short text describing what this secret is used for and how it should be obtained.
            This will be shown to the user during `codchi rebuild`.
          '';
        };
      };
    }));
    description = ''
      Secrets / variables which must be set by each Codchi user. They will be
      available in each shell session, *prefixed by `CODCHI_`*, inside this code
      machine, so don't use this for super sensitive secrets.

      Possible use cases:
      - GitHub / GitLab tokens for automatic git setup / repo cloning
    '';
    default = { };
    example = literalExpression /* nix */ ''{
      # Will be available as $CODCHI_GITLAB_TOKEN
      GITLAB_TOKEN = {
        description = '''
          Access token with read_api, read_repository & write_repository scopes. 
          Can be created here: <https://gitlab.example.com/-/user_settings/personal_access_tokens>. 
          Syntax: `oauth2:glpat-XXXXXXXXXXXXXXXXXXXX`.
        ''';
      };
    }'';
  };

}
