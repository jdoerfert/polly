; RUN: opt %loadPolly -analyze < %s | FileCheck %s
;
; FIXME: Edit the run line and add checks!
;
; XFAIL: *
;
;    int system(void);
;
;    int C;
;
;    int repr0(int *A, int N, int C) {
;R00:      if (C)
;R01:        return 0;
;
;R02:      for (int i = 0; i < N; i++)
;R03:        A[i]++;
;
;R04:      return system();
;    }
;
;    int repr1(int *A, int repr1N, int C) {
;R10:      if (C)
;R11:        return repr1N + A[-1];
;
;R12:      system();
;
;      int s = 0;
;R13:      for (int i = 0; i < repr1N; i++)
;R14:        s += A[i]++;
;
;      return s;
;    }
;
;    int *repr2(int *A, int N, int repr2C) {
;R20:      if (repr2C)
;R21:        return &A[-47];
;
;R22:      A[5]++;
;      return 0;
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
;    void test6(int *A, int N) {
;      int CL = C;
;
;      repr0(A, N, C);
;      inline0(A, N, 0);
;      int Repr0Res = repr0(A, N, CL);
;      int Repr1Res = repr1(A, Repr0Res, C);
;      inline1(A, Repr1Res, Repr0Res);
;      int *Repr2Res = repr2(A, N, C);
;      inline2(Repr2Res, N, CL);
;    }
;
source_filename = "t6.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"

@C = common global i32 0, align 4

define i32 @repr0(i32* %A, i32 %N, i32 %C) {
entry:
  br label %R00

R00:                                              ; preds = %entry
  %tobool = icmp eq i32 %C, 0
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
  %call = call i32 @system() #2
  br label %return

return:                                           ; preds = %R04, %R01
  %retval.0 = phi i32 [ 0, %R01 ], [ %call, %R04 ]
  ret i32 %retval.0
}

declare i32 @system() #1

define i32 @repr1(i32* %A, i32 %repr1N, i32 %C) {
entry:
  br label %R10

R10:                                              ; preds = %entry
  %tobool = icmp eq i32 %C, 0
  br i1 %tobool, label %if.end, label %if.then

if.then:                                          ; preds = %R10
  br label %R11

R11:                                              ; preds = %if.then
  %arrayidx = getelementptr inbounds i32, i32* %A, i64 -1
  %tmpR1 = load i32, i32* %arrayidx, align 4
  %add = add nsw i32 %tmpR1, %repr1N
  br label %return

if.end:                                           ; preds = %R10
  br label %R12

R12:                                              ; preds = %if.end
  %call = call i32 @system() #2
  br label %R13

R13:                                              ; preds = %R12
  %tmp1 = sext i32 %repr1N to i64
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %R13
  %indvars.iv = phi i64 [ %indvars.iv.next, %for.inc ], [ 0, %R13 ]
  %s.0 = phi i32 [ 0, %R13 ], [ %add2, %for.inc ]
  %cmp = icmp slt i64 %indvars.iv, %tmp1
  br i1 %cmp, label %for.body, label %for.end

for.body:                                         ; preds = %for.cond
  br label %R14

R14:                                              ; preds = %for.body
  %arrayidx1 = getelementptr inbounds i32, i32* %A, i64 %indvars.iv
  %tmp2 = load i32, i32* %arrayidx1, align 4
  %inc = add nsw i32 %tmp2, 1
  store i32 %inc, i32* %arrayidx1, align 4
  br label %for.inc

for.inc:                                          ; preds = %R14
  %add2 = add nsw i32 %s.0, %tmp2
  %indvars.iv.next = add nuw nsw i64 %indvars.iv, 1
  br label %for.cond

for.end:                                          ; preds = %for.cond
  %s.0.lcssa = phi i32 [ %s.0, %for.cond ]
  br label %return

return:                                           ; preds = %for.end, %R11
  %retval.0 = phi i32 [ %add, %R11 ], [ %s.0.lcssa, %for.end ]
  ret i32 %retval.0
}

define i32* @repr2(i32* %A, i32 %N, i32 %repr2C) {
entry:
  br label %R20

R20:                                              ; preds = %entry
  %tobool = icmp eq i32 %repr2C, 0
  br i1 %tobool, label %if.end, label %if.then

if.then:                                          ; preds = %R20
  br label %R21

R21:                                              ; preds = %if.then
  %arrayidx = getelementptr inbounds i32, i32* %A, i64 -47
  br label %return

if.end:                                           ; preds = %R20
  br label %R22

R22:                                              ; preds = %if.end
  %arrayidx1 = getelementptr inbounds i32, i32* %A, i64 5
  %tmp = load i32, i32* %arrayidx1, align 4
  %inc = add nsw i32 %tmp, 1
  store i32 %inc, i32* %arrayidx1, align 4
  br label %return

return:                                           ; preds = %R22, %R21
  %retval.0 = phi i32* [ %arrayidx, %R21 ], [ null, %R22 ]
  ret i32* %retval.0
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
  %call = call i32 @system() #2
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

define void @test6(i32* %A, i32 %N) {
entry:
  %CLoadTest = load i32, i32* @C, align 4
  %call = call i32 @repr0(i32* %A, i32 %N, i32 %CLoadTest)
  call void @inline0(i32* %A, i32 %N, i32 0)
  %call1 = call i32 @repr0(i32* %A, i32 %N, i32 %CLoadTest)
  %CLoadTest1 = load i32, i32* @C, align 4
  %call2 = call i32 @repr1(i32* %A, i32 %call1, i32 %CLoadTest1)
  call void @inline1(i32* %A, i32 %call2, i32 %call1)
  %CLoadTest2 = load i32, i32* @C, align 4
  %call3 = call i32* @repr2(i32* %A, i32 %N, i32 %CLoadTest2)
  ;call void @inline2(i32* %call3, i32 %N, i32 %tmp)
  ret void
}

