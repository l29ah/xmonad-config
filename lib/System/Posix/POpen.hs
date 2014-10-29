--  popen-like library
--
--  Author : Jens-Ulrik Petersen
--  Created: 16 August 2001
--
--  Version:  $Revision: 1.5 $ from $Date: 2001/10/17 07:30:53 $
--
--  Copyright (c) 2001 Jens-Ulrik Holger Petersen
--  (c) The GRASP/AQUA Project, Glasgow University, 1995-1996
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Library General Public
--  License as published by the Free Software Foundation; either
--  version 2 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Library General Public License for more details.
--
-- Description
--

-- This code is based on runProcess from the hslibs posix
-- library, but internally it uses file descriptors instead
-- of handles and returns the output and error streams
-- lazily as strings as well as the pid of forked process,
-- instead of just IO ().

module System.Posix.POpen (popen, popenEnvDir)
where

import System.Posix.Types (Fd(), ProcessID())
import System.Posix.IO (createPipe, fdToHandle, closeFd, dupTo, stdInput, stdOutput, stdError)
-- import Control.Concurrent (forkIO)
import System.Posix.Process (executeFile)
import System.Directory (setCurrentDirectory)
import System.IO (hGetContents, hPutStr, hClose)
import Data.Maybe (fromJust, isJust)
import Control.Monad (when)

popen :: FilePath                        -- Command
      -> [String]                        -- Arguments
      -> Maybe String                    -- Input
      -> IO (String, String, ProcessID)  -- (stdout, stderr, pid)
popen path args inpt =
    popenEnvDir path args inpt Nothing Nothing

popenEnvDir :: FilePath                  -- Command
              -> [String]                        -- Arguments
              -> Maybe String                    -- Input
              -> Maybe [(String, String)]        -- Environment
              -> Maybe FilePath                  -- Working directory
                        -- (stdin, stdout, stderr, pid)
              -> IO (String, String, ProcessID)
popenEnvDir path args inpt env dir =
    do
    inr <- if (isJust inpt) then
             do
             (inr', inw) <- createPipe
             hin <- fdToHandle inw
             hPutStr hin $ fromJust inpt
             hClose hin
             return $ Just inr'
            else
            return Nothing
    (outr, outw) <- createPipe
    (errr, errw) <- createPipe
--    pid <- forkIO
    p <- doTheBusiness inr outw errw
    do
          -- close other end of pipes in here
          when (isJust inr) $
               closeFd $ fromJust inr
          closeFd outw
          closeFd errw
          hout <- fdToHandle outr
          outstrm <- hGetContents hout
          herr <- fdToHandle errr
          errstrm <- hGetContents herr
          return (outstrm, errstrm, (third p))
    where
    third (_,_,c) = c
    doTheBusiness ::
        Maybe Fd            -- stdin
        -> Fd               -- stdout
        -> Fd               -- stderr
        -> IO (String, String, ProcessID)    -- (stdout, stderr)
    doTheBusiness inr outw errw =
        do
        maybeChangeWorkingDirectory dir
        when (isJust inr) $
             dupTo (fromJust inr) stdInput >> return ()
        dupTo outw stdOutput
        dupTo errw stdError
        executeFile path True args env
        -- for typing, should never actually run
        error "executeFile failed!"

maybeChangeWorkingDirectory :: Maybe FilePath -> IO ()
maybeChangeWorkingDirectory dir =
    case dir of
             Nothing -> return ()
             Just x  -> setCurrentDirectory x
