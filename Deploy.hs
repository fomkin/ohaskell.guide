{-# LANGUAGE OverloadedStrings #-}

{-
    Deploy the book to GitHub.

    $ stack ghc -- Deploy.hs
    $ ./Deploy "Commit message"

    or

    $ stack exec runhaskell Deploy.hs "Commit message"
-}


{-
import           Shelly
import qualified Data.Text              as T
import           Control.Monad          (void)
import           System.Environment     (getArgs)
import           Control.Monad.IO.Class (liftIO)
import           Control.Exception.Base

main :: IO ()
main = void . shelly $ do
    args <- liftIO getArgs
    if length args /= 1 then commitMessagePlease else do
        let [commitMessageRaw] = args
            commitMessage      = T.pack commitMessageRaw

        echo "Собираем новую версию книги..."
        run "ohaskell" []

        echo "Учитываем изменения в ветке 'master'..."
        gitAdd ["."]
        gitCommit [commitMessage]
        gitPush ["master"]

        echo "Копируем во временное место, предварительно удалив старое, если нужно..."
        rm_rf "/tmp/_site" `catch_sh` ifNot
        cp_r "_site" "/tmp"

        echo "Переключаемся на ветку 'gh-pages'..."
        gitCheckout ["gh-pages"]

        echo "Удаляем ненужное..."
        rm_f  "*.html"
        rm_rf "static"
        rm_f  "*.md"
        rm_f  "*.cabal"
        rm_f  "*.hs"

        echo "Копируем..."
        cp_r "/tmp/_site/." "."

        rm_rf "chapters"
        rm_rf "src"
        rm_rf "templates"
        rm_rf "epub"
        rm_rf "pdf"
        rm_rf "_site"
        rm_rf "_cache"
        rm_f  "*.md"
        rm_f  "*.cabal"
        rm_f  "*.hs"
        rm_f  "*.config"
        rm_f  "Deploy"

        echo "Учитываем все изменения и публикуем на GitHub Pages..."
        gitAdd ["."]
        gitCommit [commitMessage] `catch_sh` ifNot
        gitPush ["gh-pages"] `catch_sh` ifNot

        echo "Возвращаемся в ветку 'master'..."
        gitCheckout ["master"]

        echo "Готово!"
  where
    commitMessagePlease = liftIO . putStrLn $
        "Сообщение о коммите забыли, нужно ./Deploy \"Что-нибудь интересное.\""

    gitAdd      = command_ "git" ["add"]
    gitCommit   = command_ "git" ["commit", "-a", "-m"]
    gitPush     = command_ "git" ["push", "origin"]
    gitCheckout = command_ "git" ["checkout"]

    ifNot :: SomeException -> Sh ()
    ifNot _ = return ()
-}






module Main where

import Control.Monad            (unless)
import Data.List                (intercalate)
import Data.Monoid              ((<>))
import System.Directory.Extra   ( doesDirectoryExist
                                , withCurrentDirectory
                                , removeDirectoryRecursive
                                )
import System.Process           (callProcess, readProcess)
import System.IO.Temp           (withSystemTempDirectory)
import System.Exit

main :: IO ()
main = do
    putStrLn $ "Собираем новую версию книги..."

    shouldBeInRepoRoot
    branchShouldBeMaster

    originUrl <- git ["config", "remote.origin.url"]
    userEmail <- git ["config", "user.email"]
    headId <- git ["rev-parse", "HEAD"]

    withSystemTempDirectory "github-pages-deploy." $ \tmpDir -> do
        putStrLn $ "Готовим временный клон во временном каталоге '" ++ tmpDir ++ "'..."
        git_ [ "clone"
             , "--config=user.email=" <> userEmail
             , "--no-checkout"
             , "."
             , tmpDir
             ]

        withCurrentDirectory tmpDir $ do
            putStrLn $ "Собираем все варианты книги во временном каталоге '" ++ tmpDir ++ "'..."
            compileBook
            rebuildBook

            -- Теперь в каталоге _site лежит веб-версия, а также файлы:
            -- pdf/ohaskell.pdf
            -- pdf/ohaskell-mobile.pdf
            -- epub/ohaskell.epub

            storeFilesInSite

            --git_ ["add", "--verbose", "."]
            --git_ ["commit", "--quiet", "--reuse-message=" <> headId]
            --git_ ["push", "--force", originUrl, "master:gh-pages"]

        --removeDirectoryRecursive tmpDir

        --git_ ["fetch"]
  where
    shouldBeInRepoRoot = doesDirectoryExist ".git" >>= \inRepoRoot ->
        unless inRepoRoot $ die "Отсутствует .git-каталог, а он мне очень нужен!"

    branchShouldBeMaster = readFile ".git/HEAD" >>= \headValue ->
        unless (strip headValue == "ref: refs/heads/master") $
            die $ "Я желаю ветку master, а вовсе не '" ++ show headValue ++ "'..."

    git args = strip <$> readProcess "git" args ""

    strip = intercalate "\n" . lines

    git_ = callProcess "git"

    compileBook = callProcess "stack" ["build"]
    rebuildBook = callProcess "stack" ["exec", "--", "ohaskell"]

    storeFilesInSite = do
        callProcess "mkdir" ["-p", "_site/pdf"]
        callProcess "cp" ["pdf/*.pdf", "_site/pdf/"]

        callProcess "mkdir" ["-p", "_site/epub"]
        callProcess "cp" ["epub/*.epub", "_site/epub/"]

