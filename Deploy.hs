{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

{-
    Деплоим книгу на GitHub (включая Pages).

    $ stack ghc -- Deploy.hs
    $ ./Deploy "Сообщение о коммите"

    или

    $ stack exec runhaskell Deploy.hs "Сообщение о коммите"
-}

module Main where

import Control.Monad            (unless)
import Data.List                (intercalate)
import System.Directory         ( createDirectory
                                , copyFile
                                , doesDirectoryExist
                                , removeDirectoryRecursive
                                )
import System.FilePath.Posix    ((</>))
import System.Process           (callProcess)
import System.Exit              (die)
import System.Environment       (getArgs)

main :: IO ()
main = do
    putStrLn $ "Собираем новую версию книги..."

    shouldBeInRepoRoot
    branchShouldBeMaster

    compileBook
    commitNPushToMasterIfNecessary
    rebuildBook

    -- Артефакты сборки (в ветке master не учитываются):
    -- _site                    -> полная веб-версия
    -- pdf/ohaskell.pdf         -> PDF A4
    -- pdf/ohaskell-mobile.pdf  -> PDF A5
    -- epub/ohaskell.epub       -> EPUB

    storeArtefactsInSite
    saveSiteInTempDirectory
    checkoutToGhPages
    resetLastCommit             -- Во избежание накопления истории в ветке gh-pages.
    takeSiteFromTempDirectory
    commitNPushToGhPages
    removeTempDirectory
    backToMaster
  where
    shouldBeInRepoRoot = doesDirectoryExist ".git" >>= \inRepoRoot ->
        unless inRepoRoot $ die "Отсутствует .git-каталог, а он мне очень нужен!"

    branchShouldBeMaster = readFile ".git/HEAD" >>= \headValue ->
        unless (strip headValue == "ref: refs/heads/master") $
            die $ "Я желаю ветку master, а вовсе не '" ++ show headValue ++ "'..."
      where
        strip = intercalate "\n" . lines

    git_ = callProcess "git"

    commitNPushToMasterIfNecessary = do
        arguments <- getArgs
        if | null arguments -> do
               putStrLn "Сообщения о коммите нет, считаем, что в ветке master нет локальных изменений."
               return ()
           | length arguments == 1 -> do
               putStrLn "Учитываем изменения в ветке master..."
               let [commitMessage] = arguments
               git_ ["commit", "-a", "-m", commitMessage]
               git_ ["push", "origin", "master"]
           | otherwise -> die $
               "Запускайте с одним сообщением о коммите, или совсем без него."

    commitNPushToGhPages = do
        putStrLn "Учитываем изменения в ветке gh-pages..."
        git_ ["add", "."]
        git_ ["commit", "-a", "-m", "Current."]
        -- git_ ["push", "-f", "origin", "gh-pages"]

    compileBook = (putStrLn $ "Компилируем...") >> callProcess "stack" ["build"]
    rebuildBook = (putStrLn $ "Собираем...")    >> callProcess "stack" ["exec", "--", "ohaskell"]

    fullWeb = "_site"
    storeArtefactsInSite = do
        createDirectory $ fullWeb </> "pdf"
        copyFile "pdf/ohaskell.pdf"         $ fullWeb </> "pdf"
        copyFile "pdf/ohaskell-mobile.pdf"  $ fullWeb </> "pdf"

        createDirectory $ fullWeb </> "/epub"
        copyFile "epub/ohaskell.epub"       $ fullWeb </> "epub"

    saveSiteInTempDirectory     = callProcess "cp" ["-R", fullWeb, "/tmp"]
    checkoutToGhPages           = git_ ["checkout", "gh-pages"]
    resetLastCommit             = git_ ["reset", "--hard", "HEAD~1"]
    takeSiteFromTempDirectory   = callProcess "cp" ["-R", "/tmp" </> fullWeb ++ "/.", "."]
    removeTempDirectory         = removeDirectoryRecursive $ "/tmp" </> fullWeb
    backToMaster                = git_ ["checkout", "master"]

