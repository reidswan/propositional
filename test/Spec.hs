import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Propositional.Engine
import Propositional.Parser
import qualified Propositional.Parser.Normal as Norm
import Propositional.AST
import Data.Either (isLeft)


-------------------------
-- Fixtures for engine --
-------------------------
knowledge_base = [
      Atom "a"
    , Atom "b"
    ]

knowledge_base_complex = [
    Implies (Atom "a") (And (Atom "b") (Atom "c")),
    Atom "a"
    ]

knowledge_base_inconsistent = [Atom "a", Neg $ Atom "a"]

engineTest = do
    describe "Engine.query" $ do
        it "entails a ^ b when kb contains [a, b]" $ do
            query knowledge_base (And (Atom "a") (Atom "b")) `shouldBe` True
        it "entails a | b when kb contains [a, b]" $ do
            query knowledge_base (Or (Atom "a") (Atom "b")) `shouldBe` True
        it "does not entail ~a when kb contains a" $ do
            query knowledge_base (Neg $ Atom "a") `shouldBe` False
        it "does not entail atoms not mentioned in the kb" $ do
            query knowledge_base (Atom "arb") `shouldBe` False
        it "entails non-atomic statement in the kb" $ do
            query knowledge_base_complex (Implies (Atom "a") (And (Atom "b") (Atom "c"))) `shouldBe` True
        it "entails composite consequences" $ do
            query knowledge_base_complex (And (Atom "b") (Atom "c")) `shouldBe` True
        it "entails tautologies" $ do
            query [] (Or (Atom "a") (Neg $ Atom "a"))
        it "entails anything when kb is inconsistent" $ do
            query knowledge_base_inconsistent (Atom "arbitrary") `shouldBe` True

-------------------------
-- Fixtures for parser --
-------------------------

src1 = "a -> (b -> a)"
parsed1 = Implies (Atom "a") (Implies (Atom "b") (Atom "a"))

src2 = "a -> ?"

src3 = "a ^ (b & c)"
parsed3 = And (Atom "a") (And (Atom "b") (Atom "c"))

src4a = "a       ^        b\n"
src4b = "a^b"

src5 = "a ^ b\n some other stuff"
parsed5 = And (Atom "a") (Atom "b")

precedence = "a ^ b ^ c -> a | b ^ c"
precedenceParsed = Implies (And (Atom "a") (And (Atom "b") (Atom "c"))) (Or (Atom "a") (And (Atom "b") (Atom "c")))

parenthesised = "a ^ ~((b -> ~ (d -> a & b)) | d ^ ~a)"
parenthesisedParsed = 
    (And (Atom "a")
         (Neg (Or (Implies (Atom "b") 
                           (Neg (Implies (Atom "d")
                                         (And (Atom "a")
                                              (Atom "b")))))
                  (And (Atom "d")
                       (Neg (Atom "a"))))))

multiline = "a ^ b\nc -> ~a\nc"
multilineParsed = [
      And (Atom "a") (Atom "b")
    , Implies (Atom "c") (Neg $ Atom "a")
    , Atom "c"
    ]

multilineWithGarbage = "a ^ b\nc->d\ngarbage goes here"

normalNoImpl = "(a -> b)"

normalWff = "(~a & (b | ~b))"
normalWffParsed = NormAnd (NormNegAtom "a") (NormOr (NormAtom "b") (NormNegAtom "b"))

multiNormal = "(~a | ~b)\na\n~b"
multiNormalParsed = [
      NormOr (NormNegAtom "a") (NormNegAtom "b")
    , NormAtom "a"
    , NormNegAtom "b"
    ]

multiNormalWithGarbage = "a\ngarbage goes here"

parserTest = do
    describe "Parser.parseProposition" $ do
        it "should parse an atom" $ do
            parseProposition "a" `shouldBe` Right (Atom "a")
        it "should parse material implication" $ do
            parseProposition src1 `shouldBe` Right parsed1
        it "should not parse malformed implication" $ do
            parseProposition src2 `shouldSatisfy` isLeft
        it "should accept both conjunction symbols" $ do
            parseProposition src3 `shouldBe` Right parsed3
        it "should ignore whitespace" $ do
            parseProposition src4a `shouldBe` parseProposition src4b
        it "should parse up to newline" $ do
            parseProposition src5 `shouldBe` Right parsed5
        it "should respect precedence" $ do
            parseProposition precedence `shouldBe` Right precedenceParsed
        it "should parse parentheses" $ do
            parseProposition parenthesised `shouldBe` Right parenthesisedParsed
    describe "Parser.parsePropositions" $ do
        it "should parse multiple statements" $ do
            parsePropositions multiline `shouldBe` Right multilineParsed
        it "should fail if there is garbage at the end" $ do
            parsePropositions multilineWithGarbage `shouldSatisfy` isLeft
    describe "Parser.Normal.parseProposition" $ do
        it "should not accept material implication" $ do
            Norm.parseProposition normalNoImpl `shouldSatisfy` isLeft
        it "should not accept non-negation-normal formulae" $ do
            Norm.parseProposition "~(a ^ b)" `shouldSatisfy` isLeft
        it "should parse well-formed negation normal formula" $ do
            Norm.parseProposition normalWff `shouldBe` Right normalWffParsed
    describe "Parser.Normal.parsePropositions" $ do
        it "should parse multiple formulae" $ do
            Norm.parsePropositions multiNormal `shouldBe` Right multiNormalParsed
        it "should fail on garbage end" $ do
            Norm.parsePropositions multiNormalWithGarbage `shouldSatisfy` isLeft


main = hspec $ do
    engineTest
    parserTest