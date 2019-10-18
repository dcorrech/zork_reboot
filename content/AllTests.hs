module AllTests where

import TestUtils
import Test.HUnit

import NaturalLanguageLexerTests
import NaturalLanguageParserTests
import NaturalLanguageProcessingIntegrationTests

------- Tests ---------
-- NOTE: Must download the Test.HUnit package to run.

allTests = TestList [TestLabel "NaturalLanguageProcessingIntegrationTests" naturalLanguageProcessingIntegrationTests,
                     TestLabel "naturalLanguageParserTests" naturalLanguageParserTests,
                     TestLabel "naturalLanguageLexerTests" naturalLanguageLexerTests]