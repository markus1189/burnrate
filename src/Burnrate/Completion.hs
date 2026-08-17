-- | Tab-completion for SPEC arguments, mirroring 'Burnrate.Spec.parseSpec'.
--
--   Each clause hands back the *whole* set of candidate words for its mode and
--   lets the completion engine (optparse-applicative) do the prefix matching,
--   so a user typing `pass:api/re` gets the engine filtering the full pass
--   listing. The one exception is that a globally-matching `**`/`*` spec path
--   (e.g. `pass:api/*`) stays literal here: word lists never auto-glob, exactly
--   as in @parseSpec.expand@, which only expands a glob passed to `run`.
module Burnrate.Completion
  ( completeSpec,
    specCompleter,
  )
where

import Burnrate.Spec (envEntries, fileEntries, passEntries)
import Data.List (isPrefixOf)
import Options.Applicative (Completer, mkCompleter)

-- | Candidate SPEC words for a partially-typed argument. A bare token is a
--   @pass@ entry (like a bare `parseSpec` argument); explicit modes only
--   complete the part after the colon and re-attach the prefix, so the words
--   are valid SPECs for the parser.
completeSpec :: String -> IO [String]
completeSpec w
  | "pass:" `isPrefixOf` w = prep "pass:" passEntries
  | "env:" `isPrefixOf` w = prep "env:" envEntries
  | "file:" `isPrefixOf` w = prep "file:" (fileEntries (drop 5 w))
  | "cmd:" `isPrefixOf` w = pure [] -- free-form, nothing to offer
  | otherwise = passEntries -- bare = pass, like parseSpec
  where
    -- Re-attach the mode prefix to every candidate so the words are valid
    -- SPECs. The prefix is a plain 'String', so no IsString ambiguity leaks;
    -- optparse filters the full candidate list by prefix.
    prep :: String -> IO [String] -> IO [String]
    prep p = fmap (map (p ++))

-- | The optparse 'Completer' for the SPEC argument. optparse performs the
--   actual prefix filtering against what the shell sends on
--   @--bash-completion-word@.
specCompleter :: Completer
specCompleter = mkCompleter completeSpec
