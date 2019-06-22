self: super: with super.haskell.lib; { 
  haskellPackages = super.haskellPackages.override (old: {
    overrides = super.lib.composeExtensions (old.overrides or (_: _: {})) (self: super: {
      servant-elm = self.callHackage "servant-elm" "0.6.0.2" {};
    });
  });
}
