 * Parse Givens:
 
   1. All KnownNats and KnownDims -> put into a dict of evidence.
      Try simplifying to individual variables?
   2. All Inequalities -> Normalize and put into a dict
   
   
   

Possible inputs in Givens/Wanteds:

KnownNat exp, KnownDim exp, exp1 ~ exp2, exp1 >= exp2, exp1 > exp2 ...: knowns (not fixed to numbers) and (in)equalities

All of the above can be either what we have or what we want to have.


First I need to do is to tranfsorm all of these into a normalized form.
e.g:

replace all exps with single variables (or funs? can be negative?..)

Change all exps to the normal form and substract rhs from lhs to get one of: e >=0 , e == 0, e > 0


