{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.FileFormat
    ( FileFormat(..)
    , PdfVer(..)
    , getFileformat
    , getFileformatFrom
    , isBinaryFile
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Char8 ()
import Data.List
import Data.Word

import qualified Control.Exception as E
import Control.Applicative ((<$>))

import System.IO
import System.IO.Error
import Text.Printf (printf)

data PdfVer = PDF14 | PDF15 | PDF16
    deriving (Show,Eq)

data FileFormat =
    -- programs & libraries
      FT_ELF
    | FT_AR
    -- container/compression
    | FT_GZIP
    | FT_ZIP
    -- document formats
    | FT_PDF PdfVer
    -- disk drive formats
    | FT_QEMU_QCOW
    | FT_VHD
    | FT_VHDX
    | FT_PartTable
    -- image formats
    | FT_JPEG
    | FT_JPEG_EXIF
    | FT_JFIF_EXIF
    | FT_PNG
    -- media formats & containers
    | FT_MP3
    | FT_OGG
    | FT_AVI
    | FT_RIFF
    | FT_DEX
    | FT_Sqlite3
    -- programmation language dependent files
    | FT_CAML_CMI
    | FT_Python_Compiled
    | FT_Haskell_Interface
    | FT_JavaClass_Compiled
    -- others
    | FT_Text
    | FT_Bittorrent
    | FT_Directory
    | FT_Unknown ByteString
    deriving (Eq)

instance Show FileFormat where
    show FT_ELF = "ELF"
    show FT_AR = "AR"
    show FT_GZIP = "GZIP"
    show FT_ZIP = "ZIP"
    show (FT_PDF pdfver) = show pdfver
    show FT_QEMU_QCOW = "QCOW"
    show FT_VHD = "VHD"
    show FT_VHDX = "VHDX"
    show FT_PartTable = "Partition table"
    show FT_JPEG = "JPEG"
    show FT_JPEG_EXIF = "EXIF JPEG"
    show FT_JFIF_EXIF = "EXIF JFIF"
    show FT_AVI = "AVI"
    show FT_PNG = "PNG"
    show FT_MP3 = "MP3"
    show FT_OGG = "OGG"
    show FT_RIFF = "RIFF"
    show FT_DEX = "android DEX"
    show FT_Sqlite3 = "Sqlite3 database"
    show FT_CAML_CMI = "Caml interface"
    show FT_Python_Compiled = "Python compiled"
    show FT_Haskell_Interface = "Haskell interface"
    show FT_JavaClass_Compiled = "JavaClass compiled"
    show FT_Text = "text"
    show FT_Bittorrent = "bittorrent"
    show FT_Directory = "directory"
    show (FT_Unknown v) = printf "unknown x%02xx%02xx%02xx%02x" (getI 0) (getI 1) (getI 2) (getI 3)
        where getI i
                | B.length v > i = B.index v i
                | otherwise      = 0

fileList :: [(FileFormat, (Int, ByteString))]
fileList =
    [ (FT_PartTable,         (0x1fe, "\x55\xaa"))
    , (FT_ELF,               (0, "\x7f\x45\x4c\x46"))
    , (FT_PNG,               (0, "\137PNG"))
    , (FT_JPEG,              (0, "\255\216\255\224\NUL\DLE"))
    , (FT_JPEG_EXIF,         (0, "\xFF\xD8\xFF\xE1"))
    , (FT_JFIF_EXIF,         (0, "\xFF\xD8\xFF\xE0"))
    , (FT_CAML_CMI,          (0, "Caml1999I"))
    , (FT_Haskell_Interface, (0, "\SOH\250\206d"))
    , (FT_PDF PDF15,         (0, "%PDF-1.5"))
    , (FT_PDF PDF16,         (0, "%PDF-1.6"))
    , (FT_PDF PDF14,         (0, "%PDF-1.4"))
    , (FT_QEMU_QCOW,         (0, "QFI\251"))
    , (FT_VHD,               (0, "conectix"))
    , (FT_VHDX,              (0, "vhdxfile"))
    , (FT_MP3,               (0, "ID3\EOT"))
    , (FT_OGG,               (0, "OggS"))
    , (FT_AVI,               (0, "\x52\x49\x46\x46\xf2\x99\x6e\x00\x41\x56\x49\x20"))
    , (FT_RIFF,              (0, "RIFF"))
    , (FT_Python_Compiled,   (0, "\209\242\r\n"))
    , (FT_GZIP,              (0, "\US\139\b\NUL"))
    , (FT_ZIP,               (0, "PK\ETX\EOT"))
    , (FT_AR,                (0, "!<arch>\n"))
    , (FT_DEX,               (0, "dex\n"))
    , (FT_Sqlite3,           (0, "SQLite format 3"))
    , (FT_JavaClass_Compiled,(0, "\xCA\xFE\xBA\xBE"))
    , (FT_Bittorrent,        (0, "d7:comment"))
    , (FT_Bittorrent,        (0, "d4:info"))
    , (FT_Bittorrent,        (0, "d8:announce"))
    ]

readFileStart :: FilePath -> Int -> IO ByteString
readFileStart file n = E.bracket (openFile file ReadMode) hClose (flip B.hGet n)

getFileformatFrom :: ByteString -> FileFormat
getFileformatFrom bs = maybe checkText fst $ find f fileList
    where
        f (_, (offset, exbs)) = exbs `B.isPrefixOf` B.drop offset bs
        checkText
            | getBinaryStat bs > 0.0 = FT_Unknown bs
            | otherwise              = FT_Text

getFileformat :: FilePath -> IO FileFormat
getFileformat file = (getFileformatFrom <$> readFileStart file 512)
         `E.catch` (\(e :: E.IOException) -> if ioeGetErrorString e == "inappropriate type" then return FT_Directory else E.throwIO e)

getBinaryStat :: ByteString -> Double
getBinaryStat bs = B.foldl' (\acc w -> acc + if isBin w then 1 else 0) 0 bs / (fromIntegral $ B.length bs)
    where
        isBin :: Word8 -> Bool
        isBin i
            | i >= 0 && i <= 8   = True
            | i == 12            = True
            | i >= 14 && i <= 31 = True
            | otherwise          = False

isBinaryFile :: FilePath -> IO Bool
isBinaryFile file = do
    bs <- readFileStart file 512
    return (getBinaryStat bs > 0.0)
