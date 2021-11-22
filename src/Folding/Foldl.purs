module RMRK.Folding.Foldl where

import Prelude

import Data.Either (Either(..))

-- !! NB: 
-- this should be moved to seperat package
-- just keeping it for reference here for now

data VerificationState
  = Unverified
  | PendingReview
  | Verified
  | Rejected Reason

data VerificationEvent
  = Verify
  | Reject Reason
  | RequestReview

data Reason = Reason String
data TransitionError = TransitionError String

transition :: VerificationState -> VerificationEvent -> Either TransitionError VerificationState

transition (Unverified) RequestReview = Right PendingReview
transition (Unverified) Verify = Left $ TransitionError "cannot verify a non pending verification request"
transition (Unverified) (Reject (Reason _)) = Left $ TransitionError "cannot reject allready unverified"

transition (PendingReview) (Reject (Reason s)) = Right $ Rejected $ Reason s
transition (PendingReview) Verify = Right Verified
transition (PendingReview) RequestReview = Left $ TransitionError "cannot request review for allready pending state"

transition (Verified) (Reject (Reason s)) = Right $ Rejected $ Reason s
transition (Verified) Verify = Left $ TransitionError "cannot reverify allreqady verified state"
transition (Verified) RequestReview = Left $ TransitionError "cannot request review for allready verified collection"

transition (Rejected (Reason _)) RequestReview = Right PendingReview
transition (Rejected (Reason _)) (Reject (Reason _)) = Left $ TransitionError "cannot reject allready rejected verification"
transition (Rejected (Reason _)) Verify = Left $ TransitionError "cannot verify rejected collection verification"