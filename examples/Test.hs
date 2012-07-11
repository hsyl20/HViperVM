import ViperVM

main = do
  putStrLn "Hello"
  cl <- loadOpenCL "/usr/lib/libOpenCL.so"
  n <- getNumPlatforms cl
  putStrLn $ show $ n
