from prolog_structures import Rule, RuleBody, Term, Function, Variable, Atom, Number
from typing import List
from functools import reduce

import sys
import random

class Not_unifiable(Exception):
	pass

'''
Please read prolog_structures.py for data structures
that represent Prolog terms, rules, and goals.
'''
class Interpreter:
	def __init__(self):
		pass

	'''
	Example
	occurs_check (v, t) where v is of type Variable, t is of type Term.
	occurs_check (v, t) returns true if the Prolog Variable v occurs in t.
	Please see the lecture note Control in Prolog to revisit the concept of
	occurs-check.
	'''

	def occurs_check (self, v : Variable, t : Term) -> bool:
		if isinstance(t, Variable):
			return v == t
		elif isinstance(t, Function):
			for t in t.terms:
				if self.occurs_check(v, t):
					return True
			return False
		return False

	'''
	Problem 1
	variables_of_term (t) where t is of type Term.
	variables_of_clause (c) where c is of type Rule.

	The function should return the Variables contained in a term or a rule
	using Python set.

	The result must be saved in a Python set. The type of each element (a Prolog Variable)
	in the set is Variable.
	'''

	def variables_of_term (self, t : Term) -> set :
		
		res = []

		if isinstance(t, Variable):	 
			res.append(t)			
		elif isinstance(t, Function): 
			for tm in t.terms:
				if isinstance(tm, Variable):
					res.append(tm)

		return set(res)

	def variables_of_clause (self, c : Rule) -> set :
		
		res = []

		if isinstance(c, Rule):  
			for tm_h in c.head.terms:
				if isinstance(tm_h, Variable): 
					res.append(tm_h)
			
			if isinstance(c.body, RuleBody):
				if (c.body.terms != []): 
					for f in c.body.terms:
						for tm_bf in f.terms:
							if isinstance(tm_bf, Variable): 
								res.append(tm_bf)

		return set(res)

	'''
	Problem 2
	substitute_in_term (s, t) where s is of type dictionary and t is of type Term
	substitute_in_clause (s, t) where s is of type dictionary and c is of type Rule,

	The value of type dict should be a Python dictionary whose keys are of type Variable
	and values are of type Term. It is a map from variables to terms.

	The function should return t_ obtained by applying substitution s to t.

	Please use Python dictionary to represent a subsititution map.
	'''

	def substitute_in_term (self, s : dict, t : Term) -> Term:
		if isinstance(t, Function):
			new_terms = []
			for tm in t.terms:
				if tm in s:
					new_terms.append(s[tm])		
				else:
					new_terms.append(tm)
	
			return Function(t.relation, new_terms)

		elif isinstance(t, Variable):
			if t in s:
				return s[t]
			else:
				return Variable(t.value)

		return t # should not reach here

	def substitute_in_clause (self, s : dict, c : Rule) -> Rule:
		if isinstance(c, Rule):
			new_terms_h = []
			for tm_h in c.head.terms:
				if tm_h in s:
					new_terms_h.append(s[tm_h])
				else:
					new_terms_h.append(tm_h)
			
			if isinstance(c.body, RuleBody):
				new_terms_body = []
				if c.body.terms != []:
					for bf in c.body.terms:
						new_terms_bf = []
						for tm_bf in bf.terms:
							if tm_bf in s:
								new_terms_bf.append(s[tm_bf])
							else:
								new_terms_bf.append(tm_bf)
						
						new_terms_body.append(Function(bf.relation, new_terms_bf))
			
				return Rule((Function(c.head.relation, new_terms_h)), RuleBody(new_terms_body))
			
		return c # should not reach here

	'''
	Problem 3
	unify (t1, t2) where t1 is of type term and t2 is of type Term.
	The function should return a substitution map of type dict,
	which is a unifier of the given terms. You may find the pseudocode
	of unify in the lecture note Control in Prolog useful.

	The function should raise the exception raise Not_unfifiable (),
	if the given terms are not unifiable.

	Please use Python dictionary to represent a subsititution map.
	'''

	def unify (self, t1: Term, t2: Term) -> dict:
		def unify_helper (X: Term, Y: Term, substt_dict):

			X = self.substitute_in_term(substt_dict, X)
			Y = self.substitute_in_term(substt_dict, Y)

			if isinstance(X, Variable) and (X not in self.variables_of_term(Y)):
				# replace X with Y in the substitution terms of 𝜃, add X/Y to 𝜃
				for key in substt_dict:
					substt_dict[key] = self.substitute_in_term({X:Y}, substt_dict[key])
	
				substt_dict[X] = Y	
				return substt_dict

			elif isinstance(Y, Variable) and (Y not in self.variables_of_term(X)):
				# replace Y with X in the substitution terms of 𝜃 add Y/X to 𝜃
				for key in substt_dict:
					substt_dict[key] = self.substitute_in_term({Y:X}, substt_dict[key])

				substt_dict[Y] = X
				return substt_dict

			elif ((isinstance(X, Variable) and isinstance(Y, Variable)) or (isinstance(X, Number) and isinstance(Y, Number))
					or (isinstance(X, Atom) and isinstance(Y, Atom))) and (X.value == Y.value):
				return substt_dict

			elif isinstance(X, Function) and isinstance(Y, Function):
				itr = [(t_x, t_y) for t_x, t_y in zip(X.terms, Y.terms)]
				for t_x, t_y in itr:
					substt_dict = unify_helper(t_x, t_y, substt_dict)

				return substt_dict

			else:
				raise Not_unifiable()

		return unify_helper(t1, t2, {})

	fresh_counter = 0
	def fresh(self) -> Variable:
		self.fresh_counter += 1
		return Variable("_G" + str(self.fresh_counter))

	def freshen(self, c: Rule) -> Rule:
		c_vars = self.variables_of_clause(c)
		theta = {}
		for c_var in c_vars:
			theta[c_var] = self.fresh()

		return self.substitute_in_clause(theta, c)

	'''
	Problem 4
	Following the Abstract interpreter pseudocode in the lecture note Control in Prolog to implement
	a nondeterministic Prolog interpreter.

	nondet_query (program, goal) where
		the first argument is a program which is a list of Rules.
		the second argument is a goal which is a list of Terms.

	The function returns a list of Terms (results), which is an instance of the original goal and is
	a logical consequence of the program. See the tests cases (in src/main.py) as examples.
	'''

	def nondet_query (self, program : List[Rule], pgoal : List[Term]) -> List[Term]:
		return []

	'''
	Challenge Problem

	det_query (program, goal) where
		the first argument is a program which is a list of Rules.
		the second argument is a goal which is a list of Terms.

	The function returns a list of term lists (results). Each of these results is
	an instance of the original goal and is a logical consequence of the program.
	If the given goal is not a logical consequence of the program, then the result
	is an empty list. See the test cases (in src/main.py) as examples.
	'''
	
	def det_query (self, program : List[Rule], pgoal : List[Term]) -> List[List[Term]]:
		return [pgoal]
