import Options.Applicative

data Options = Options {
    missingnessThreshold :: Double,
    verbose              :: Bool,
    summaryStat          :: SummaryStatSpec,
    inputFormat          :: FormatSpec,
    individuals          :: IndividualsSpec
} deriving (Show)

data SummaryStatSpec = Heterozygosity | SegregatingSites | HardyWeinbergDeviation deriving (Show)

data FormatSpec = PlinkFormat FilePath FilePath FilePath | VCFFormat FilePath deriving (Show)

data IndividualsSpec = IndividualsByFile FilePath | IndividualsByList [String] deriving (Show)

missingnessParser :: Parser Double
missingnessParser = option auto (long "missingness" <> short 'm')

verboseParser :: Parser Bool
verboseParser = switch (long "verbose" <> short 'v')

summaryStatParser :: Parser SummaryStatSpec
summaryStatParser = hetParser <|> segSitesParser <|> hwParser
  where
    hetParser      = flag' Heterozygosity (long "heterozygosity")
    segSitesParser = flag' SegregatingSites (long "segregatingSites")
    hwParser       = flag' HardyWeinbergDeviation (long "hardyWeinbergDev")

plinkFormatParser :: Parser FormatSpec
plinkFormatParser = PlinkFormat <$> genoParser <*> snpParser <*> indParser
  where
    genoParser = strOption (long "genoFile")
    snpParser  = strOption (long "snpFile")
    indParser  = strOption (long "indFile")

vcfFormatParser :: Parser FormatSpec
vcfFormatParser = VCFFormat <$> vcfFileParser
  where
    vcfFileParser = strOption (long "vcfFile")

formatParser :: Parser FormatSpec
formatParser = plinkFormatParser <|> vcfFormatParser

individualsParser :: Parser IndividualsSpec
individualsParser = individualsFileParser <|> individualsListParser
  where
    individualsFileParser = IndividualsByFile <$> strOption (long "individualsFile")
    individualsListParser = IndividualsByList <$> some (strOption (long "ind"))

optionsParser :: Parser Options
optionsParser = Options <$> missingnessParser <*> verboseParser <*> summaryStatParser <*> formatParser <*> individualsParser

main :: IO ()
main = do
    opts <- execParser opts
    print opts
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )