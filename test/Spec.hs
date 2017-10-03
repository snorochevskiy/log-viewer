import Slf4jLogs

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
  defaultMain (testGroup "Slf4jLogs Tests" [isNewLogLineTest, takeBlockTest])


isNewLogLineTest :: TestTree
isNewLogLineTest = testGroup "Testing isNewLogLine" [

  testCase "Checking line that is a new log entry"
    (assertEqual "Should consider as a new log entry start" True
    $ isNewLogLine $ BS.pack "02:59:06.246 34007377 [http-nio-8080-exec-9] INFO"),

  testCase "Checking line that is a part of a log entry"
    (assertEqual "Should not consider as a new log entry start" False
    $ isNewLogLine $ BS.pack "java.lang.NullPointerException: null")

  ]

takeBlockTest :: TestTree
takeBlockTest = testGroup "Testing takeBlock" [

  testCase "Taking single log entry"
    (assertEqual "Should consider as a new log entry start"
    (BS.pack "02:59:06.246 34007377 [http-nio-8080-exec-9] INFO  com.xya.Aaa - Some message\nMessage Continues",
     BS.pack "")
    $ takeBlock $ BS.pack "02:59:06.246 34007377 [http-nio-8080-exec-9] INFO  com.xya.Aaa - Some message\nMessage Continues"),

  testCase "Taking first log entry from multiple"
    (assertEqual "Should consider as a new log entry start"
    (BS.pack "02:59:06.246 34007377 [http-nio-8080-exec-9] INFO  com.xya.Aaa - A",
     BS.pack "\n02:59:06.246 34007377 [http-nio-8080-exec-9] INFO  com.xya.Aaa - B")
    $ takeBlock $ BS.pack "02:59:06.246 34007377 [http-nio-8080-exec-9] INFO  com.xya.Aaa - A\n02:59:06.246 34007377 [http-nio-8080-exec-9] INFO  com.xya.Aaa - B")

  ]
