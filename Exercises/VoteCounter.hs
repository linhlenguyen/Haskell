module VoteCounter(
main
)
  where
    import Data.Map
    import System.Random
    import Control.Monad
    import Control.Monad.Reader

    type UserId = Int
    type PlayerId = Int

    data User = User {
      uid :: Int,
      uname :: String
    }

    data Player = Player {
      pid :: Int,
      pname :: String
    }

    type UserToVote = Map User Int
    type VoteToPlayer = Map Int Player

    voteNum :: Int
    voteNum = 1000000

    maxUserId :: UserId
    maxUserId = 1000000

    ranGen :: StdGen
    ranGen = mkStdGen 27

    users :: [UserId]
    users = take voteNum $ randomRs (1,maxUserId) ranGen

    votes :: [PlayerId]
    votes = take voteNum $ randomRs (1,5) ranGen

    usersToVotes :: [(UserId, PlayerId)]
    usersToVotes = zip users votes

    toMap :: [(UserId, PlayerId)] -> (Map UserId [PlayerId], Map PlayerId Int)
    toMap ls = Prelude.foldl addVote (empty,empty) ls

    addVote :: (Map UserId [PlayerId], Map PlayerId Int) -> (UserId, PlayerId) -> (Map UserId [PlayerId], Map PlayerId Int)
    addVote (m,mp) (userId, playerId) = let x = Data.Map.lookup userId m
                                            incPid = modifyVoteToPlayer mp (\p -> p+1)
                                            decPid = modifyVoteToPlayer mp (\p -> p-1) in
                                         case x of {
                                            Nothing -> (insert userId [playerId] m, incPid playerId);
                                            Just ps -> if (length ps == 3) then (m, mp)
                                              else (insert userId (playerId:ps) m, incPid playerId);
                                         }

    modifyVoteToPlayer :: Map PlayerId Int -> (PlayerId -> PlayerId) -> PlayerId -> Map PlayerId Int
    modifyVoteToPlayer m f pid = let x = Data.Map.lookup pid m in
                                  case x of {
                                      Nothing -> insert pid 1 m;
                                      Just a -> insert pid (f a) m;
                                  }

    voteCount :: Map UserId [PlayerId] -> Map PlayerId Int
    voteCount m = let v = concat $ elems m in Prelude.foldl foldf empty v
      where foldf :: Map PlayerId Int -> PlayerId -> Map PlayerId Int
            foldf m pid = let x = Data.Map.lookup pid m in
                          case x of {
                              Nothing -> insert pid 1 m;
                              Just a -> insert pid (a+1) m;
                          }

    main :: IO ()
    main = putStrLn $ show $ snd $ toMap $ usersToVotes
