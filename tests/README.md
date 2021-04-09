# Exosphere Tests

## Installation

To run tests, you'll need to install the Exosphere development dependencies:

```
npm install
```

## Getting Started

You can use the elm-test README as a way to orient to structure of tests:

https://github.com/elm-community/elm-test#quick-start

## Example, a single test

Exploring one example might be helpful in pointing out some of the intending structure.

```elm
-- simple test suites need to import the following:
import Expect exposing (Expectation)
import Test exposing (..)

testPalindrome : Test
testPalindrome =
    test "has no effect on a palindrome" <|
        \() ->
            let
                palindrome =
                    "hannah"
            in
                Expect.equal palindrome (String.reverse palindrome)
```

The `String` passing to `test` function is used to provide "output" for when there is a failure. It is common to use of the left pipeline operator, `<|`, as it avoids having to wrap the anonymous function in parentheses. The use of `()` (aka "Unit") is meant to flag, or "signal" (a person _waving_ to you), that nothing will be done with this argument.

If you explore other projects for examples of tests, you will see that some use "the hockey stick", \_, for the anonymous function. There are individuals that argue this is supposed to be "less clear" since you're accepting an argument, and that argument for a `Test` will be disregarded or ignored.

## Example, structuring tests using `describe`

You can group together tests and example their functions using `describe`.

```elm
testPalindromes : Test
testPalindromes =
    describe "the overall test suite"
        [ describe "simple palindrome cases without spaces"
            [ test "has no effect on a palindrome" <|
                \() ->
                    let
                        palindrome =
                            "hannah"
                    in
                        Expect.equal palindrome (String.reverse palindrome)
            ]
        , describe "palindromed cases where spaces must be never"
            [ test "has no effect on a palindrome" <|
                \() ->
                    let
                        normalize s =
                            String.split " " s |> String.join ""

                        palindrome =
                            "was it a rat i saw"
                    in
                        palindrome
                            |> normalize
                            |> Expect.equal (String.reverse (normalize palindrome))
            ]
        ]
```
