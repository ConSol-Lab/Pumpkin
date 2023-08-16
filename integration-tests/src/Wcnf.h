/***********[Wcnf.h]
Copyright (c) 2020, Fahiem Bacchus

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
***********/

#ifndef WCNF_H
#define WCNF_H

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <iomanip>
#include <iostream>
#include <limits>
#include <set>
#include <string>
#include <vector>

inline bool blank_ln(std::string ln) {
  // also true for empty line
  return (std::all_of(ln.begin(), ln.end(),
                      [](char x) { return std::isspace(x); }));
}

class Wcnf {
public:
  using llint = int64_t;
  void loadFromStream(std::istream &inStream);
  void compute_pline();
  void output_stat_lines(std::ostream &outStream) const;
  void outputInstance(std::ostream &outStream) const;
  void outputCNF(std::ostream &outStream, bool hards,
                 const std::string &file_name) const;
  void set_exact_top(bool et) { exact_top = et; };
  void set_old_fmt(bool of) { old_fmt = of; };
  void set_preserve(bool p) { preserve = p; };
  llint maxVar() const { return nvars; }

  // Format of model: model[i] == -1 ==> var[i] = false, model[i] == 1
  // ==> var[i] = true, model[i] == 0 ==> var[i] unset. Model should
  // have size maxVar+1 (there is no variable 0). Return model cost,
  // return -1 if model fails to satisfy hards, return -2 if an error
  // occured
  llint get_model_cost(const std::vector<int8_t> &model,
                       std::set<size_t> &unsat_softs) const;

private:
  llint var(llint l) const { return std::abs(l); }
  size_t varIdx(llint l) const { return static_cast<size_t>(var(l)); }

  void parse_p_line(std::string &ln);
  void parse_clause(std::string &ln);
  llint get_wt(const char *&cur_pos, const std::string &ln);
  void get_lits(std::vector<llint> &lits, const char *&cur_pos,
                const std::string &ln);
  bool is_hard(llint wt) const {
    return (exact_top && wt == top) || (!exact_top && wt >= top);
  }
  bool is_soft(llint wt) const { return !is_hard(wt); }
  bool bad_wt(llint wt) const {
    return (!exact_top && wt < 0) || (exact_top && wt != top && wt < 0);
  }
  bool warn_wt(llint wt) const {
    // some input instances used top = 0! Handle such instances by using
    // exact_top
    return (!exact_top && wt == 0) || (exact_top && wt != top && wt == 0);
  }

  void compute_blo_top();
  std::vector<llint> find_blo_wts() const;
  // vector used to input clauses
  std::vector<llint> input_cls{};

  llint top{}, nvars{}, ncls{}, f_nvars{}, f_ncls{}, sum_soft_wts{0};
  std::set<llint> unique_wts;
  int nwarns1{}, nwarns2{};
  bool isWcnf{false}, instance_empty{true};
  bool exact_top{false}, preserve{false};
  static bool old_fmt; // to share with operator<< printing clauses
  bool wcnf_no_top{false};
  bool seen_p_line{false};
  bool seen_clause{false};
  std::string sha1;

  std::vector<std::string> comment_lines;
  std::vector<char> line_type;
  constexpr static char c_line{'c'};
  constexpr static char p_line{'p'};
  constexpr static char cls_line{'l'};

  struct Clause {
    bool hard{false};
    llint wt{};
    std::vector<llint> lits;
  };

  std::vector<Clause> clauses;
  friend std::ostream &operator<<(std::ostream &os, const Clause &);
  struct Stats {
    llint n{0};
    llint total{0};
    llint max{0};
    llint min{0};
    long double ave{0};
    long double varN{0};
    void update(llint x) {
      if (x > max || n == 0)
        max = x;
      if (x < min || n == 0)
        min = x;
      ++n;
      total += x;
      auto prev_ave = ave;
      ave = ave + (x - ave) / n;
      varN = varN + (x - prev_ave) * (x - ave);
    }
    long double stdev() const { return n == 0 ? 0 : std::sqrt(varN / n); }
  };
  Stats soft_lens, hard_lens, weights, future;
  void update_stats(bool hard, llint wt, const std::vector<llint> &lits);
  void adjust_wts();
  void remap_vars();
};

#endif
