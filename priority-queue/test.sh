#!/bin/bash

ghc Test.hs -o Test -O2 && echo OK && ./Test
