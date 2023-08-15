module Language.Printf.Options where

data Extensions = CStandard
                | POSIXStandard
                | Extensions
                | OpinionatedExtensions
                deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- C standard: only C (C24)
-- POSIX standard: include POSIX extensions: %1$'d
-- Extensions: include Haskell extensions: %v, %@, %=X, %Czi, %~b, %R12i, ...
-- Opinionated extensions: Warn about bad old behavior (just %#x, %#o, ... and %B (always) for now, I think)

-- Alternative to %Ux: introduce signed variants/flag
--   %b, %B, %o, %x, %X
--   %r, %R, %q, %y, %Y?  R is from binaRy, q is from opQ, y is from xY
--   %±b, %±B, %±o, %±x, %±X?  ± isn't really ideal, though – what to use instead?  s and S are out
--   %_b, %_B, %_o, %_x, %_X?  Weird, but works, and could also give the right behavior with %#_x and 0
--   %~b, %~B, %~o, %~x, %~X?  Honestly, even better.  %#~x?  ~ is probably a flag, then.  %~#x?  %+~x?
--   %=b, %=B, %=o, %=x, %=X?  Also not bad.  Could use for an all-lowercase #, though, so %=x would
--                             give the "best" behavior (then %=B would be banned -- only matters for X)

-- So, alternative to incompatibility:
--   ~ indicates "treat as signed" for bBoxX
--   = is a "better #" (but still uppercase, boo) – synonym? or only for bBoxX?
--   < could be "always lowercase =" and > "always uppercase =":
--     %<X is the "right way" to print, %>x would look real goofy
--     <> definitely only for bBoxX, but:
--       possibly not o at all
--       possibly < only for BX, > only for bx
--       probably treat like # and warn
--   or: super opinionated, = is "better #" and prefixes both always and in lowercase for boxX,
--       including o just so it can be a replacement for #
--   warming to this tbh
--   %~=X or %=~X prints -0xFF, which is great, if long.
--   %=X prints 0xFF, which is nice and short
--   %~x prints -ff, which isn't bad either
-- But does that steal a lot of valuable real estate?
--
-- Maybe:
--   = is a "better #", and I get to be opinionated and make it lowercase
--     Question: Does = affect floating-point behavior, like # does?
--   I right after the size: sIgned, like %i
--   Thus: %=IX prints -0xFF, %=X prints 0xFF, everyone is happy
--   Maximal HS extensions: %=CIX prints a signed CInt in uppercase hex with the 0x prefix even if it's 0
--                          %=ClIX is the same for CLong, etc.
--
-- Flags don't change the type, or I could use %@ and %#@ / %v and %#v for full-function printing
-- But full-function would want to see #, so probably best to stick with %@ (and maybe %&) / %v and %V
--
-- Would like to be able to specify, e.g., %R7d to print an int in base (Radix) 7.
-- But if that's a flag, we can't do it – flags come before widths, so
-- "% R7 7 d" is indistinguishable from "% R77 d".  Could put it where the I extension is --
-- %8R4i prints 8 digits of 4-bit output (signed), and %8R4u prints 8 digits of 4-bit output (unsigned),
-- and R4 is incompatible with I as well as with anything besides i/u (and probably d, even though d is for
-- decimal – maybe warn there).  Also, of course, %R*i and %R*1$i.
--
-- Oh, but then I need `%R12I` and `%R12U` for uppercase integers (warning if
-- the base ≤ 10).  That's fine but conflicts with `%Ix` for signed hex!  Hmm.
-- of "SIGNED", S, I, G, E, and D are out, so that just leaves N… but that's
-- fine?  It conflicts with `%n` but we don't support that anyway.  Only problem
-- is it looks weird.  `%Nx`?  Not obvious.  Could go back to making it a flag –
-- "~" indicating "treat as signed".  This feels best.  `%=~x`, but `%=R16i`;
-- `%=x`, but `%=R16u`.  I like it!  So the `R` part is genuinely a new
-- component, like `1$` – the "base" component.
-- Stretch goal: floating point.

-- Okay, but `~` is weird as a flag, because it changes the types.  New
-- proposal: just use `I`.  `%x` is the same as `%R16u` and `%X` is the same as
-- `%R16U`; the old `%~x` becomes `%R16i` and the old `%~X` becomes `%R16I`.
-- Downsides: `%~x` is much shorter.  Realistically, though, who cares?  `%Nx`
-- isn't that much better, and is hard to read anyway.
