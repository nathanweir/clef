//! Zed extension for Common Lisp, backed by the clef language server.
//!
//! Zed's job here is small on purpose: tell Zed how to start clef, and let the
//! server do the rest over LSP. Anything clef can answer -- highlighting,
//! outline, navigation -- should be answered by clef rather than duplicated in
//! this extension or in a tree-sitter query.

use zed_extension_api::{self as zed, LanguageServerId, Result, Worktree};

struct CommonLispExtension;

impl zed::Extension for CommonLispExtension {
    fn new() -> Self {
        Self
    }

    fn language_server_command(
        &mut self,
        _language_server_id: &LanguageServerId,
        worktree: &Worktree,
    ) -> Result<zed::Command> {
        let lsp_settings = zed::settings::LspSettings::for_worktree("clef", worktree)
            .ok()
            .unwrap_or_default();

        // An explicit path in settings wins. This is how you point Zed at a
        // build in a working tree rather than whatever is on PATH.
        let command = match lsp_settings.binary.as_ref().and_then(|b| b.path.as_ref()) {
            Some(path) => path.clone(),
            None => self.find_clef_command(worktree)?,
        };

        let args = lsp_settings
            .binary
            .and_then(|b| b.arguments)
            .unwrap_or_default();

        Ok(zed::Command {
            command,
            args,
            env: Default::default(),
        })
    }
}

impl CommonLispExtension {
    fn find_clef_command(&self, worktree: &Worktree) -> Result<String> {
        worktree.which("clef").ok_or_else(|| {
            "Clef LSP not found. Install clef and ensure it is on your PATH, \
             or set the path in settings: \
             {\"lsp\": {\"clef\": {\"binary\": {\"path\": \"/path/to/clef\"}}}}"
                .to_string()
        })
    }
}

zed::register_extension!(CommonLispExtension);

// Removed: language_server_workspace_configuration.
//
// It sent clef a block of settings that were all inherited from the alive-lsp
// era and that clef does not read:
//
//   "alive.format": {...}          alive-lsp's formatter options
//   "commonlisp.trace.server"      a VS Code tracing switch
//   "editor.semanticTokenColorCustomizations"
//                                  VS Code's theme override mechanism, which
//                                  Zed does not implement; token colours in Zed
//                                  come from the theme, keyed by the token
//                                  types clef advertises in its legend
//   "editor.formatOnType": true    an editor setting, not a server one, and
//                                  clef advertises no onTypeFormatting anyway
//
// Sending configuration a server ignores is not harmless: it reads as though
// the feature is wired up. If clef ever wants configuration it should define
// its own keys and this hook can come back for those.
