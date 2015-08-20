theory "Lem_support" 

imports 
 	 Main 
begin 

subsection {* Finite sets *}

typedef 'a finset = "{s::'a set. finite s}"
  morphisms set_from_finset finset_from_set
proof
  show "{} \<in> {s. finite s}" by simp
qed

declare set_from_finset_inverse[simp]
lemmas finset_from_set_inverse[simp] = finset_from_set_inverse[simplified]

definition list_from_finset :: "'a finset \<Rightarrow> 'a list" where
  "list_from_finset fs = (SOME l. (set l = set_from_finset fs))"

lemma finite_set_from_finset [simp] :
  "finite (set_from_finset fs)"
using set_from_finset[of fs] by simp

lemma set_list_from_finset [simp] :
  "set (list_from_finset fs) = set_from_finset fs"
proof -
  have "finite (set_from_finset fs)"
    by simp
  hence "\<exists>l. set l = set_from_finset fs"
    by (rule finite_list)
  thus ?thesis
    unfolding list_from_finset_def
    by (rule someI_ex)
qed


definition finset_from_list :: "'a list \<Rightarrow> 'a finset" where
  "finset_from_list l = finset_from_set (set l)"

lemma finset_from_list_inv [simp] :
  "finset_from_list (list_from_finset fs) = fs"
unfolding finset_from_list_def 
by simp

lemma set_from_finset_from_list [simp]: 
  "set_from_finset (finset_from_list l) = set l"
unfolding finset_from_list_def by simp

lemmas finset_from_set_inject [simp] = finset_from_set_inject[simplified]

lemma list_from_finset_eq_nil[simp]: 
   "(list_from_finset fs = []) = (fs = finset_from_set {})"
unfolding list_from_finset_def
proof (rule someI2_ex)
  show "\<exists>a. set a = set_from_finset fs"
    by (rule finite_list) simp
next
  fix s
  assume set_s_eq: "set s = set_from_finset fs"

  show "(s = []) = (fs = finset_from_set {})"
  using set_s_eq
  by (metis set_empty2 set_from_finset_from_list set_from_finset_inverse)
qed


end
