; RUN: opt %loadPolly -analyze < %s | FileCheck %s
;
; FIXME: Edit the run line and add checks!
;
; XFAIL: *
;
;
;    void f(int *A, int i, int k) {
;      for (int j = 0; j < A[k]; j++)
;        A[j + i] = i;
;    }
;
;    int *G;
;    void loop(int N) {
;      for (int i = 0; i < N; i++)
;        f(G, i, i);
;    }
;
source_filename = "t.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"

@G = common global i32* null, align 8

define void @f(i32* %A, i32 %i, i32 %k) {
entry:
  %tmp = sext i32 %i to i64
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %indvars.iv = phi i64 [ %indvars.iv.next, %for.inc ], [ 0, %entry ]
  %idxprom = sext i32 %k to i64
  %arrayidx = getelementptr inbounds i32, i32* %A, i64 %idxprom
  %tmp2 = load i32, i32* %arrayidx, align 4
  %tmp3 = sext i32 %tmp2 to i64
  %cmp = icmp slt i64 %indvars.iv, %tmp3
  br i1 %cmp, label %for.body, label %for.end

for.body:                                         ; preds = %for.cond
  %tmp4 = add nsw i64 %indvars.iv, %tmp
  %arrayidx2 = getelementptr inbounds i32, i32* %A, i64 %tmp4
  store i32 %i, i32* %arrayidx2, align 4
  br label %for.inc

for.inc:                                          ; preds = %for.body
  %indvars.iv.next = add nuw i64 %indvars.iv, 1
  br label %for.cond

for.end:                                          ; preds = %for.cond
  ret void
}

define void @loop(i32 %N) {
entry:
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %i.0 = phi i32 [ 0, %entry ], [ %inc, %for.inc ]
  %cmp = icmp slt i32 %i.0, %N
  br i1 %cmp, label %for.body, label %for.end

for.body:                                         ; preds = %for.cond
  %tmp = load i32*, i32** @G, align 8
  call void @f(i32* %tmp, i32 %i.0, i32 %i.0)
  br label %for.inc

for.inc:                                          ; preds = %for.body
  %inc = add nuw nsw i32 %i.0, 1
  br label %for.cond

for.end:                                          ; preds = %for.cond
  ret void
}
