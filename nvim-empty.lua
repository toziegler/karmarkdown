local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
  {
    "nvim-telescope/telescope.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      local telescope = require("telescope")
      telescope.setup({})
      local builtin = require("telescope.builtin")
      vim.keymap.set("n", "<leader>ws", builtin.lsp_workspace_symbols, { desc = "Workspace symbols" })
      vim.keymap.set("n", "<leader>ds", builtin.lsp_document_symbols, { desc = "Document symbols" })
    end,
  },
})

vim.api.nvim_create_autocmd("FileType", {
  pattern = "markdown",
  callback = function()
    vim.lsp.start({
      name = "huntsman",
      cmd = { "/tmp/huntsman/zig-out/bin/huntsman" },
      root_dir = vim.fn.getcwd(),
    })
  end,
})
