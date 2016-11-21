; RUN: opt %loadPolly -analyze < %s | FileCheck %s
;
; FIXME: Edit the run line and add checks!
;
; XFAIL: *
;
;    void system();
;
;    int C,Q;
;
;    void repr0(int *A, int N, int C) {
;R00:      if (Q == 7)
;R01:        return;
;
;R02:      for (int i = 0; i < N; i++)
;R03:        A[i]++;
;
;R04:      system();
;    }
;
;    void repr1(int *A, int N, int C) {
;R10:      if (C)
;R11:        return;
;
;R12:      system();
;
;R13:      for (int i = 0; i < N; i++)
;R14:        A[i]++;
;    }
;
;    void repr2(int *A, int N, int C) {
;R20:      if (C)
;R21:        return;
;
;R22:      A[5]++;
;    }
;
;    void inline0(int *A, int N, int C) {
;I00:      for (int i = 0; i < N; i++)
;I01:        A[i]++;
;    }
;
;    void inline1(int *A, int N, int C) {
;I10:      if (C)
;I11:        return;
;
;I12:      for (int i = 0; i < N; i++)
;I13:        A[i]++;
;    }
;
;    void inline2(int *A, int N, int C) {
;I20:      if (!C)
;I21:        system();
;
;I22:      for (int i = 0; i < N; i++)
;I23:        A[i]++;
;    }
;
;    void test(int *A, int N) {
;      int CL = C;
;
;      repr0(A, N);
;      inline0(A, N, 0);
;      repr1(A, N, C);
;      inline1(A, N, C);
;      repr1(A, N, CL);
;      repr2(A, N, C);
;      inline2(A, N, CL);
;      repr2(A, N, CL);
;    }
;
source_filename = "t5.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"

@C = common global i32 0, align 4
@Q = common global i32 0, align 4

define void @repr0(i32* %A, i32 %N) {
entry:
  %repr0Q = load i32, i32* @Q, align 4
  br label %R00

R00:                                              ; preds = %entry
  %tobool = icmp eq i32 %repr0Q, 7
  br i1 %tobool, label %if.end, label %if.then

if.then:                                          ; preds = %R00
  br label %R01

R01:                                              ; preds = %if.then
  br label %return

if.end:                                           ; preds = %R00
  br label %R02

R02:                                              ; preds = %if.end
  %tmp = sext i32 %N to i64
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %R02
  %indvars.iv = phi i64 [ %indvars.iv.next, %for.inc ], [ 0, %R02 ]
  %cmp = icmp slt i64 %indvars.iv, %tmp
  br i1 %cmp, label %for.body, label %for.end

for.body:                                         ; preds = %for.cond
  br label %R03

R03:                                              ; preds = %for.body
  %arrayidx = getelementptr inbounds i32, i32* %A, i64 %indvars.iv
  %tmp1 = load i32, i32* %arrayidx, align 4
  %inc = add nsw i32 %tmp1, 1
  store i32 %inc, i32* %arrayidx, align 4
  br label %for.inc

for.inc:                                          ; preds = %R03
  %indvars.iv.next = add nuw nsw i64 %indvars.iv, 1
  br label %for.cond

for.end:                                          ; preds = %for.cond
  br label %R04

R04:                                              ; preds = %for.end
  call void (...) @system() #2
  br label %return

return:                                           ; preds = %R04, %R01
  ret void
}

declare void @system(...) #1

define void @repr1(i32* %A, i32 %N, i32 %C) {
entry:
  br label %R10

R10:                                              ; preds = %entry
  %tobool = icmp eq i32 %C, 0
  br i1 %tobool, label %if.end, label %if.then

if.then:                                          ; preds = %R10
  br label %R11

R11:                                              ; preds = %if.then
  br label %for.end

if.end:                                           ; preds = %R10
  br label %R12

R12:                                              ; preds = %if.end
  call void (...) @system() #2
  br label %R13

R13:                                              ; preds = %R12
  %tmp = sext i32 %N to i64
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %R13
  %indvars.iv = phi i64 [ %indvars.iv.next, %for.inc ], [ 0, %R13 ]
  %cmp = icmp slt i64 %indvars.iv, %tmp
  br i1 %cmp, label %for.body, label %for.end.loopexit

for.body:                                         ; preds = %for.cond
  br label %R14

R14:                                              ; preds = %for.body
  %arrayidx = getelementptr inbounds i32, i32* %A, i64 %indvars.iv
  %tmp1 = load i32, i32* %arrayidx, align 4
  %inc = add nsw i32 %tmp1, 1
  store i32 %inc, i32* %arrayidx, align 4
  br label %for.inc

for.inc:                                          ; preds = %R14
  %indvars.iv.next = add nuw nsw i64 %indvars.iv, 1
  br label %for.cond

for.end.loopexit:                                 ; preds = %for.cond
  br label %for.end

for.end:                                          ; preds = %for.end.loopexit, %R11
  ret void
}

define void @repr2(i32* %A, i32 %N, i32 %C) {
entry:
  br label %R20

R20:                                              ; preds = %entry
  %tobool = icmp eq i32 %C, 0
  br i1 %tobool, label %if.end, label %if.then

if.then:                                          ; preds = %R20
  br label %R21

R21:                                              ; preds = %if.then
  br label %return

if.end:                                           ; preds = %R20
  br label %R22

R22:                                              ; preds = %if.end
  %arrayidx = getelementptr inbounds i32, i32* %A, i64 5
  %tmp = load i32, i32* %arrayidx, align 4
  %inc = add nsw i32 %tmp, 1
  store i32 %inc, i32* %arrayidx, align 4
  br label %return

return:                                           ; preds = %R22, %R21
  ret void
}

define void @inline0(i32* %A, i32 %N, i32 %C) {
entry:
  br label %I00

I00:                                              ; preds = %entry
  %tmp = sext i32 %N to i64
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %I00
  %indvars.iv = phi i64 [ %indvars.iv.next, %for.inc ], [ 0, %I00 ]
  %cmp = icmp slt i64 %indvars.iv, %tmp
  br i1 %cmp, label %for.body, label %for.end

for.body:                                         ; preds = %for.cond
  br label %I01

I01:                                              ; preds = %for.body
  %arrayidx = getelementptr inbounds i32, i32* %A, i64 %indvars.iv
  %tmp1 = load i32, i32* %arrayidx, align 4
  %inc = add nsw i32 %tmp1, 1
  store i32 %inc, i32* %arrayidx, align 4
  br label %for.inc

for.inc:                                          ; preds = %I01
  %indvars.iv.next = add nuw nsw i64 %indvars.iv, 1
  br label %for.cond

for.end:                                          ; preds = %for.cond
  ret void
}

define void @inline1(i32* %A, i32 %N, i32 %C) {
entry:
  br label %I10

I10:                                              ; preds = %entry
  %tobool = icmp eq i32 %C, 0
  br i1 %tobool, label %if.end, label %if.then

if.then:                                          ; preds = %I10
  br label %I11

I11:                                              ; preds = %if.then
  br label %for.end

if.end:                                           ; preds = %I10
  br label %I12

I12:                                              ; preds = %if.end
  %tmp = sext i32 %N to i64
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %I12
  %indvars.iv = phi i64 [ %indvars.iv.next, %for.inc ], [ 0, %I12 ]
  %cmp = icmp slt i64 %indvars.iv, %tmp
  br i1 %cmp, label %for.body, label %for.end.loopexit

for.body:                                         ; preds = %for.cond
  br label %I13

I13:                                              ; preds = %for.body
  %arrayidx = getelementptr inbounds i32, i32* %A, i64 %indvars.iv
  %tmp1 = load i32, i32* %arrayidx, align 4
  %inc = add nsw i32 %tmp1, 1
  store i32 %inc, i32* %arrayidx, align 4
  br label %for.inc

for.inc:                                          ; preds = %I13
  %indvars.iv.next = add nuw nsw i64 %indvars.iv, 1
  br label %for.cond

for.end.loopexit:                                 ; preds = %for.cond
  br label %for.end

for.end:                                          ; preds = %for.end.loopexit, %I11
  ret void
}

define void @inline2(i32* %A, i32 %N, i32 %C) {
entry:
  br label %I20

I20:                                              ; preds = %entry
  %tobool = icmp ne i32 %C, 0
  br i1 %tobool, label %if.end, label %if.then

if.then:                                          ; preds = %I20
  br label %I21

I21:                                              ; preds = %if.then
  call void (...) @system() #2
  br label %if.end

if.end:                                           ; preds = %I20, %I21
  br label %I22

I22:                                              ; preds = %if.end
  %tmp = sext i32 %N to i64
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %I22
  %indvars.iv = phi i64 [ %indvars.iv.next, %for.inc ], [ 0, %I22 ]
  %cmp = icmp slt i64 %indvars.iv, %tmp
  br i1 %cmp, label %for.body, label %for.end

for.body:                                         ; preds = %for.cond
  br label %I23

I23:                                              ; preds = %for.body
  %arrayidx = getelementptr inbounds i32, i32* %A, i64 %indvars.iv
  %tmp1 = load i32, i32* %arrayidx, align 4
  %inc = add nsw i32 %tmp1, 1
  store i32 %inc, i32* %arrayidx, align 4
  br label %for.inc

for.inc:                                          ; preds = %I23
  %indvars.iv.next = add nuw nsw i64 %indvars.iv, 1
  br label %for.cond

for.end:                                          ; preds = %for.cond
  ret void
}

define void @test(i32* %testA, i32 %testN) {
entry:
  call void @repr0(i32* %testA, i32 %testN)
  call void @inline0(i32* %testA, i32 %testN, i32 0)
  %testC0 = load i32, i32* @C, align 4
  call void @repr1(i32* %testA, i32 %testN, i32 %testC0)
  %testC1 = load i32, i32* @C, align 4
  call void @inline1(i32* %testA, i32 %testN, i32 %testC1)
  call void @repr1(i32* %testA, i32 %testN, i32 %testC0)
  %testC2 = load i32, i32* @C, align 4
  call void @repr2(i32* %testA, i32 %testN, i32 %testC2)
  call void @inline2(i32* %testA, i32 %testN, i32 %testC1)
  call void @repr2(i32* %testA, i32 %testN, i32 %testC2)
  ret void
}
