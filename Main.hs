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
missingnessParser = option auto (long "missingness" <>
  short 'm' <> help "A missingness threshold" <> value 0.5 <> showDefault <> metavar "NUMBER")

verboseParser :: Parser Bool
verboseParser = switch (long "verbose" <> short 'v' <> help "verbose output")

summaryStatParser :: Parser SummaryStatSpec
summaryStatParser = hetParser <|> segSitesParser <|> hwParser
  where
    hetParser = flag' Heterozygosity (long "heterozygosity" <>
      help "compute the rate of heterozygosity for each individual")
    segSitesParser = flag' SegregatingSites (long "segregatingSites" <>
      help "compute the rate of segregating sites for each individual")
    hwParser = flag' HardyWeinbergDeviation (long "hardyWeinbergDev" <>
      help "compute the average deviation from Hardy-Weinberg equilibrium for each individual")

plinkFormatParser :: Parser FormatSpec
plinkFormatParser = PlinkFormat <$> genoParser <*> snpParser <*> indParser
  where
    genoParser = strOption (long "genoFile" <> help "the input genotype file" <> metavar "FILE")
    snpParser  = strOption (long "snpFile" <> help "the input snp file" <> metavar "FILE")
    indParser  = strOption (long "indFile" <> help "the input individual file" <> metavar "FILE")

vcfFormatParser :: Parser FormatSpec
vcfFormatParser = VCFFormat <$> vcfFileParser
  where
    vcfFileParser = strOption (long "vcfFile" <> help "the input VCF file" <> metavar "FILE")

formatParser :: Parser FormatSpec
formatParser = plinkFormatParser <|> vcfFormatParser

individualsParser :: Parser IndividualsSpec
individualsParser = individualsFileParser <|> individualsListParser
  where
    individualsFileParser = IndividualsByFile <$> strOption (long "individualsFile" <>
      help "list individuals in the file given" <> metavar "FILE")
    individualsListParser = IndividualsByList <$> some (strOption (long "ind" <>
      help "list individuals directly on the command line. Option can be given multiple times, once for each individual" <>
      metavar "NAME"))

optionsParser :: Parser Options
optionsParser = Options <$> missingnessParser <*> verboseParser <*> summaryStatParser <*> formatParser <*> individualsParser

main :: IO ()
main = do
    opts <- execParser opts
    print opts
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Hello, this is a toy example for how to design command line interfaces in Haskell"
     <> header "Haskell-CLI-Example" )