{

  globals.mapleader = ",";

  colorschemes.catppuccin.enable = true;
  plugins = {
    bufferline.enable = true;
    nvim-tree.enable = true;
    nvim-autopairs.enable = true;
    treesitter.enable = true;
    undotree.enable = true;
    which-key.enable = true;
    cmp.enable = true;
    luasnip.enable = true;
    haskell-scope-highlighting.enable = false;
    lsp = {
      enable = true;
      keymaps.diagnostic = {
        "<leader>j" = "goto_next";
        "<leader>k" = "goto_prev";
      };
      keymaps.lspBuf = {
        K = "hover";
        "<leader>af" = "format";
        "<leader>ac" = "code_action";
        gD = "references";
        gd = "definition";
        gi = "implementation";
        gt = "type_definition";
      };
      servers.nixd.enable = true;
    };
    rustaceanvim.enable = true;
    telescope = {
      enable = true;
      keymaps = {
        "<leader>fG" = {
          action = "git_files";
          desc = "Telescope Git Files";
        };
        "<leader>fg" = "live_grep";
      };
    };

  };

}
