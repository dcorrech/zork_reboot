module AllTests where

import TestUtils
import Test.HUnit

import NaturalLanguageLexerTests
import NaturalLanguageParserTests
import NaturalLanguageProcessingIntegrationTests

allTests = TestList [TestLabel "NaturalLanguageProcessingIntegrationTests" naturalLanguageProcessingIntegrationTests,
                     TestLabel "naturalLanguageParserTests" naturalLanguageParserTests,
                     TestLabel "naturalLanguageLexerTests" naturalLanguageLexerTests]