{-# LANGUAGE BinaryLiterals #-}

module Sound.Novation.Launchpad.Chars4x4 where

import           Data.Bits
import           Data.ByteString as BS hiding (foldl)
import           Data.Word

reverseByte :: Word8 -> Word8
reverseByte a =
  foldl (\b n -> if testBit a n then setBit b (l - 1 - n) else b) zeroBits [0..l]
  where
    l =
      finiteBitSize a

chars4x4 :: [Word8] -> ByteString
chars4x4 =
   BS.map reverseByte . pack

a :: ByteString
a =
  chars4x4 [
    0b1111,
    0b1001,
    0b1111,
    0b1001
  ]

b :: ByteString
b =
  chars4x4 [
    0b1000,
    0b1110,
    0b1010,
    0b1110
  ]

c :: ByteString
c =
  chars4x4 [
    0b1111,
    0b1000,
    0b1000,
    0b1111
  ]

d :: ByteString
d =
  chars4x4 [
    0b1110,
    0b1001,
    0b1001,
    0b1110
  ]

e :: ByteString
e =
  chars4x4 [
    0b1111,
    0b1110,
    0b1000,
    0b1110
  ]

f :: ByteString
f =
  chars4x4 [
    0b1111,
    0b1110,
    0b1000,
    0b1000
  ]

g :: ByteString
g =
  chars4x4 [
    0b1111,
    0b1000,
    0b1001,
    0b1111
  ]

h :: ByteString
h =
  chars4x4 [
    0b1000,
    0b1000,
    0b1110,
    0b1001
  ]

i :: ByteString
i =
  chars4x4 [
    0b1,
    0b1,
    0b1,
    0b1
  ]

j :: ByteString
j =
  chars4x4 [
    0b01,
    0b01,
    0b01,
    0b11
  ]

k :: ByteString
k =
  chars4x4 [
    0b1010,
    0b1100,
    0b1010,
    0b1001
  ]

l :: ByteString
l =
  chars4x4 [
    0b1000,
    0b1000,
    0b1000,
    0b1111
  ]

m :: ByteString
m =
  chars4x4 [
    0b1001,
    0b1111,
    0b1001,
    0b1001
  ]

n :: ByteString
n =
  chars4x4 [
    0b1001,
    0b1101,
    0b1011,
    0b1001
  ]

o :: ByteString
o =
  chars4x4 [
    0b1111,
    0b1001,
    0b1001,
    0b1111
  ]

p :: ByteString
p =
  chars4x4 [
    0b1110,
    0b1001,
    0b1110,
    0b1000
  ]

q :: ByteString
q =
  chars4x4 [
    0b1111,
    0b1001,
    0b1111,
    0b0001
  ]

r :: ByteString
r =
  chars4x4 [
    0b1110,
    0b1001,
    0b1110,
    0b1010
  ]

s :: ByteString
s =
  chars4x4 [
    0b1111,
    0b1000,
    0b0001,
    0b1111
  ]

t :: ByteString
t =
  chars4x4 [
    0b1111,
    0b0100,
    0b0100,
    0b0100
  ]

u :: ByteString
u =
  chars4x4 [
    0b1001,
    0b1001,
    0b1001,
    0b1111
  ]

v :: ByteString
v =
  chars4x4 [
    0b1001,
    0b0101,
    0b0011,
    0b0001
  ]

w :: ByteString
w =
  chars4x4 [
    0b1001,
    0b1001,
    0b1001,
    0b0110
  ]

x :: ByteString
x =
  chars4x4 [
    0b0000,
    0b1010,
    0b0100,
    0b1010
  ]

y :: ByteString
y =
  chars4x4 [
    0b1001,
    0b0110,
    0b0010,
    0b0010
  ]

z :: ByteString
z =
  chars4x4 [
    0b1111,
    0b0010,
    0b0100,
    0b1111
  ]
