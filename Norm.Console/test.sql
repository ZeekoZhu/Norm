[35mSELECT[0m
  [Id][[32m33[0mm,[[32m0[0mm
  [33m([0m[view_Count][33m)[0m [35mAS[0m [ViewCount]
[35mFROM[0m
  [blog_Comment]
  [35mLEFT[0m [35mJOIN[0m [33m([0m
    [35mSELECT[0m
      [Id][[32m33[0mm,[[32m0[0mm
      [33m([0m[view_Count][33m)[0m [35mAS[0m [ViewCount]
    [35mFROM[0m
      [blog_Content]
  [33m)[0m [35mAS[0m [Temp] [35mON[0m [33m([0m[Id] [[32m33[0mm=[[32m0[0mm [BlogId][33m)[0m
