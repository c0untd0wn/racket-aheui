# racket-aheui

## Introduction

Racket-Aheui is an Aheui interpreter written in Racket (former PLT Scheme). Aheui is an programming language that uses Hangul (Korean Characters). For more information about Aheui please refer to below.

* [아희란?](http://aheui.github.io/introduction.ko/)
* [아희의 표준 문서](http://aheui.github.io/specification.ko/)

## Usage

You need DrRacket to run this code. It gets the Aheui code input from a file. You can pass the name and path of your aheui code as an argument.

```
racket aheui.rkt YOUR_AHEUI_CODE
```

or if you create executable from DrRacket
```
aheui YOUR_AHEUI_CODE
```
is also possible.

## Test
This implementation passes all tests. logo.aheui took approximately 25 minutes on my 2012 Macbook Air.

To run tests using [Aheui snippets](https://github.com/aheui/snippets), create executable from DrRacket.

## About the current version

* Needs to be refactored to do things in a more functional way.
* More optimization required.
