{-# LANGUAGE CPP #-}
-- | The module 'UserIO' defines how to do the interaction with the user.
--   By default, we use a command-line interface (as defined by the module 'CLI').
--   To define a new mode of interaction (e.g., GUI), create a new module similar
--   to 'CLI' and export the new module in 'UserIO'.

module UserIO (
#if defined(__USE_READLINE__)
    module CLI
#else
    module CLINoReadline
#endif
) where

#if defined(__USE_READLINE__)
    import CLI
#else
    import CLINoReadline
#endif
