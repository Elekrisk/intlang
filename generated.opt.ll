; ModuleID = 'generated.ll'
source_filename = "main"

%VarStorage = type { i64, %BackingStore* }
%BackingStore = type { i64, %Value }
%Value = type { i8, [4 x i64] }

@variable_storage = local_unnamed_addr global %VarStorage zeroinitializer

; Function Attrs: inaccessiblemem_or_argmemonly mustprogress nounwind willreturn
declare noalias noundef i8* @realloc(i8* nocapture, i64 noundef) local_unnamed_addr #0

; Function Attrs: mustprogress nounwind willreturn
define void @expand_variable_storage() local_unnamed_addr #1 {
entry:
  %var-storage.unpack = load i64, i64* getelementptr inbounds (%VarStorage, %VarStorage* @variable_storage, i64 0, i32 0), align 8
  %var-storage.unpack13 = load i8*, i8** bitcast (%BackingStore** getelementptr inbounds (%VarStorage, %VarStorage* @variable_storage, i64 0, i32 1) to i8**), align 8
  %new-size = shl i64 %var-storage.unpack, 1
  %byte-size = mul i64 %var-storage.unpack, 88
  %new_ptr = tail call i8* @realloc(i8* %var-storage.unpack13, i64 %byte-size)
  %ptr-casted-to-backing-store-ptr-ptr = bitcast i8* %new_ptr to %BackingStore*
  %half-ptr = getelementptr %BackingStore, %BackingStore* %ptr-casted-to-backing-store-ptr-ptr, i64 %var-storage.unpack
  %half-void-ptr = bitcast %BackingStore* %half-ptr to i8*
  %old-byte-size = mul i64 %var-storage.unpack, 44
  tail call void @llvm.memset.p0i8.i64(i8* align 1 %half-void-ptr, i8 0, i64 %old-byte-size, i1 false)
  store i64 %new-size, i64* getelementptr inbounds (%VarStorage, %VarStorage* @variable_storage, i64 0, i32 0), align 8
  store i8* %new_ptr, i8** bitcast (%BackingStore** getelementptr inbounds (%VarStorage, %VarStorage* @variable_storage, i64 0, i32 1) to i8**), align 8
  ret void
}

; Function Attrs: mustprogress nofree nounwind willreturn
define void @initialize_variable_storage() local_unnamed_addr #2 {
entry:
  %calloc = call dereferenceable_or_null(704) i8* @calloc(i64 1, i64 704)
  store i64 16, i64* getelementptr inbounds (%VarStorage, %VarStorage* @variable_storage, i64 0, i32 0), align 8
  store i8* %calloc, i8** bitcast (%BackingStore** getelementptr inbounds (%VarStorage, %VarStorage* @variable_storage, i64 0, i32 1) to i8**), align 8
  ret void
}

; Function Attrs: nounwind
define %BackingStore* @allocate_backing_store() local_unnamed_addr #3 {
entry:
  %var-storage.unpack = load i64, i64* getelementptr inbounds (%VarStorage, %VarStorage* @variable_storage, i64 0, i32 0), align 8
  %var-storage.unpack1 = load %BackingStore*, %BackingStore** getelementptr inbounds (%VarStorage, %VarStorage* @variable_storage, i64 0, i32 1), align 8
  %0 = bitcast %BackingStore* %var-storage.unpack1 to i8*
  br label %loop

loop:                                             ; preds = %loop-continued, %entry
  %loop_var = phi i64 [ 0, %entry ], [ %incd-loop-var, %loop-continued ]
  %is-at-end-of-list.not = icmp ult i64 %loop_var, %var-storage.unpack
  br i1 %is-at-end-of-list.not, label %loop-continued, label %no-unallocated-backing-store

loop-continued:                                   ; preds = %loop
  %backing-store.elt = getelementptr %BackingStore, %BackingStore* %var-storage.unpack1, i64 %loop_var, i32 0
  %backing-store.unpack = load i64, i64* %backing-store.elt, align 4
  %is-unallocated = icmp eq i64 %backing-store.unpack, 0
  %incd-loop-var = add nuw i64 %loop_var, 1
  br i1 %is-unallocated, label %unallocated-backing-store-found, label %loop

no-unallocated-backing-store:                     ; preds = %loop
  %new-size.i = shl i64 %var-storage.unpack, 1
  %byte-size.i = mul i64 %var-storage.unpack, 88
  %new_ptr.i = tail call i8* @realloc(i8* %0, i64 %byte-size.i) #3
  %ptr-casted-to-backing-store-ptr-ptr.i = bitcast i8* %new_ptr.i to %BackingStore*
  %half-ptr.i = getelementptr %BackingStore, %BackingStore* %ptr-casted-to-backing-store-ptr-ptr.i, i64 %var-storage.unpack
  %half-void-ptr.i = bitcast %BackingStore* %half-ptr.i to i8*
  %old-byte-size.i = mul i64 %var-storage.unpack, 44
  tail call void @llvm.memset.p0i8.i64(i8* align 1 %half-void-ptr.i, i8 0, i64 %old-byte-size.i, i1 false) #3
  store i64 %new-size.i, i64* getelementptr inbounds (%VarStorage, %VarStorage* @variable_storage, i64 0, i32 0), align 8
  store i8* %new_ptr.i, i8** bitcast (%BackingStore** getelementptr inbounds (%VarStorage, %VarStorage* @variable_storage, i64 0, i32 1) to i8**), align 8
  br label %unallocated-backing-store-found

unallocated-backing-store-found:                  ; preds = %loop-continued, %no-unallocated-backing-store
  %loop_var9 = phi i64 [ %var-storage.unpack, %no-unallocated-backing-store ], [ %loop_var, %loop-continued ]
  %backing-store-ptr1 = getelementptr %BackingStore, %BackingStore* %var-storage.unpack1, i64 %loop_var9
  ret %BackingStore* %backing-store-ptr1
}

; Function Attrs: argmemonly nofree nounwind willreturn writeonly
declare void @llvm.memset.p0i8.i64(i8* nocapture writeonly, i8, i64, i1 immarg) #4

; Function Attrs: inaccessiblememonly nofree nounwind willreturn
declare noalias noundef i8* @calloc(i64 noundef, i64 noundef) local_unnamed_addr #5

attributes #0 = { inaccessiblemem_or_argmemonly mustprogress nounwind willreturn }
attributes #1 = { mustprogress nounwind willreturn }
attributes #2 = { mustprogress nofree nounwind willreturn }
attributes #3 = { nounwind }
attributes #4 = { argmemonly nofree nounwind willreturn writeonly }
attributes #5 = { inaccessiblememonly nofree nounwind willreturn }
