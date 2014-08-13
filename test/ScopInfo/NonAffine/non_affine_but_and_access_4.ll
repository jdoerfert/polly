; RUN: opt %loadPolly -analyze < %s | FileCheck %s
;
;    void jd(int *A, int c) {
;      for (int i = 0; i < 1024; i++)
;        A[i & 4] = c;
;    }
;
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"

define void @jd(i32* %A, i32 %c) {
entry:
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %i.0 = phi i32 [ 0, %entry ], [ %inc, %for.inc ]
  %exitcond = icmp ne i32 %i.0, 1024
  br i1 %exitcond, label %for.body, label %for.end

for.body:                                         ; preds = %for.cond
  %and = and i32 %i.0, 4
  %idxprom1 = sext i32 %and to i64
  %arrayidx = getelementptr inbounds i32* %A, i64 %idxprom1
  store i32 %c, i32* %arrayidx, align 4
  br label %for.inc

for.inc:                                          ; preds = %for.body
  %inc = add nsw i32 %i.0, 1
  br label %for.cond

for.end:                                          ; preds = %for.cond
  ret void
}
