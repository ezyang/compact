module Data.Compact (
    -- * The 'Compact' type
    Compact,

    -- * Compacting data
    compact,
    compactWithSharing,
    compactAdd,
    compactAddWithSharing,
    compactSized,

    -- * Inspecting a Compact
    getCompact,
    inCompact,
    isCompact,
    compactSize,
) where

import GHC.Compact
