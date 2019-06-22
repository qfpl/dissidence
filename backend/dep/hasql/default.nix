self: super: with super.haskell.lib; { 
  haskellPackages = super.haskellPackages.override (old: {
    overrides = super.lib.composeExtensions (old.overrides or (_: _: {})) (hself: hsuper: {
      hasql-pool = dontCheck (unmarkBroken hsuper.hasql-pool);
      hasql-transaction = dontCheck hsuper.hasql-transaction;
      hasql-migration = dontCheck (unmarkBroken hsuper.hasql-migration);
    });
  });
}
