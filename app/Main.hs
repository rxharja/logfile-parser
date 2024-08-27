module Main where
import Options.Applicative
import Parser.LogFile (parse)
import Data.ActivityLog

data Input = FileInput FilePath | StdInput

data Analysis = Total | Average

data App = App { input :: Input, analysis :: Analysis }

totalAnalysis :: Parser Analysis
totalAnalysis = flag' Total
  (  long "total"
  <> short 't'
  <> help "Display total time spent on activity per entry")

avgAnalysis :: Parser Analysis
avgAnalysis = flag' Average
  (  long "average"
  <> short 'a'
  <> help "Display average time spent on activity per entry")

parseAnalysis :: Parser Analysis
parseAnalysis = totalAnalysis <|> avgAnalysis

fileInput :: Parser Input
fileInput = FileInput <$> strOption
  (  long "file"
  <> short 'f'
  <> metavar "FILENAME"
  <> help "Input log file" )

stdInput :: Parser Input
stdInput = flag' StdInput
  (  long "stdin"
  <> help "Read from stdin" )

parseInput :: Parser Input
parseInput = stdInput <|> fileInput 

app :: Parser App
app = App <$> parseInput <*> parseAnalysis

getLogs :: Input -> IO String
getLogs StdInput = getContents
getLogs (FileInput file) = readFile file
 
analyze :: App -> IO ()
analyze (App inp Average) = do
  contents <- getLogs inp
  print $ averageTimeByActivity . toActivityLog <$> parse contents
analyze (App inp Total) = do
  contents <- getLogs inp
  print $ totalTimeByActivity . toActivityLog <$> parse contents

main :: IO ()
main = analyze =<< execParser opts
  where
    opts = info (app <**> helper)
      ( fullDesc
      <> progDesc "Analyze a custom TARGET log file"
      <> header "logman - a command line utility for managing and analyzing logfiles.")
