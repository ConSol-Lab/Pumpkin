/***********[Wcnf.cc]
Copyright (c) 2012-2013 Jessica Davies, Fahiem Bacchus

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
#include <algorithm>
#include <cctype>
#include <charconv>
#include <cstdlib>
#include <fstream>
#include <set>
#include <sstream>
#include <stdexcept>
#include <utility>

#include "Wcnf.h"
#include "stream_utils.h"

#ifdef BLO_TOP
#include "cadical/cadical.hpp"
#endif

using std::cerr;
using std::cout;
using std::ifstream;
using std::istream;
using std::ostream;
using std::runtime_error;
using std::string;
using std::vector;

constexpr char Wcnf::c_line;
constexpr char Wcnf::p_line;
constexpr char Wcnf::cls_line;
bool Wcnf::old_fmt{false};

void Wcnf::loadFromStream(std::istream &inStream) {
  string ln, type;
  // throw exception on inStream badbit; auto clean up
  Istream_guard guard(inStream, std::ios_base::badbit);
  bool in_header{false};
  isWcnf = true;
  instance_empty = true;
  seen_p_line = seen_clause = wcnf_no_top = false;
  while (std::getline(inStream, ln)) {
    if (blank_ln(ln))
      continue;
    else if (ln.front() == 'c') {
      if (ln.rfind("c MaxSat instance", 0) == 0 ||
          ln.rfind("c Standarized MaxSat Instance", 0) == 0)
        in_header = true;
      else if (ln.rfind("c-----------", 0) == 0)
        in_header = false;
      else if (!in_header) {
        comment_lines.push_back(std::move(ln));
        line_type.push_back(c_line);
      }
    } else if (ln.front() == 'p') {
      line_type.push_back(p_line);
      parse_p_line(ln);
    } else {
      line_type.push_back(cls_line);
      parse_clause(ln);
    }
  }
  /*cerr << clauses.size() << " clauses added\n";
  cerr << "Weights             max " << weights.max << " min " << weights.min <<
  " ave "
       << weights.ave << " stdev " << weights.stdev() << "\n";
  cerr << "Soft clause lengths max " << soft_lens.max << " min "
       << soft_lens.min << " ave " << soft_lens.ave << " stdev "
       << soft_lens.stdev() << "\n";
  cerr << "Hard clause lengths max " << hard_lens.max << " min "
       << hard_lens.min << " ave " << hard_lens.ave << " stdev "
       << hard_lens.stdev() << "\n";*/
}

template <typename T>
void parse_num(const string &ln, const char *&pos, T &num) {
  auto eol = ln.data() + ln.size();
  while (pos < eol && std::isspace(*pos))
    ++pos;
  auto [end_of_num, ec] = std::from_chars(pos, eol, num);
  pos = end_of_num;
  if (ec != std::errc())
    throw std::runtime_error("Invalid line:\"" + ln + "\"\n");
}

void parse_num(const string &ln, const char *&pos, double &num) {
  char *end_of_num;
  num = std::strtod(pos, &end_of_num);
  if (pos == end_of_num) {
    throw std::runtime_error("Invalid line:\"" + ln + "\"\n");
  }
  if (errno == ERANGE)
    throw std::runtime_error("Number out of range on line:\n\"" + ln + "\"\n");
  pos = end_of_num;
}

void Wcnf::parse_p_line(string &ln) {
  if (seen_clause)
    throw runtime_error("p-line appears after clauses: \"" + ln + "\"\n");
  if (seen_p_line)
    throw runtime_error("Double p-line: \"" + ln + "\"\n");
  size_t pos{1};
  while (std::isspace(ln[pos]))
    ++pos;
  if (ln.rfind("cnf", pos) == pos) {
    isWcnf = false;
    pos += 3;
  } else if (ln.rfind("wcnf", pos) == pos) {
    isWcnf = true;
    pos += 4;
  } else
    throw runtime_error("Bad p-line: \"" + ln + "\"\n");
  auto ptr = ln.c_str() + pos;
  parse_num(ln, ptr, f_nvars);
  parse_num(ln, ptr, f_ncls);
  if (!isWcnf)
    top = std::numeric_limits<llint>::max();
  else
    try {
      parse_num(ln, ptr, top);
    } catch (...) {
      top = std::numeric_limits<llint>::max();
      wcnf_no_top = true;
    }
  seen_p_line = true;
}

/*  std::istringstream ss(ln);
  string type;
  ss.get(), ss >> type >> f_nvars >> f_ncls;
  if (!ss)
    throw runtime_error("Bad p-line: \"" + ss.str() + "\"\n");
  else if (type == "cnf") {
    isWcnf = false;
    top = std::numeric_limits<llint>::max();
  } else if (type == "wcnf") {
    isWcnf = true;
    ss >> top;
    if (!ss) {
      wcnf_no_top = true;
      top = std::numeric_limits<llint>::max();
    }
  } else
    throw runtime_error("Invalid pline: \"" + ss.str() + "\"\n");
  seen_p_line = true;
  // cerr << "p " << type << " nvars = " << f_nvars << " ncls = " << f_ncls
  //     << " top = " << top << '\n';
  }*/

Wcnf::llint Wcnf::get_wt(const char *&cur_pos, const string &ln) {
  if (isWcnf) {
    llint wt;
    parse_num(ln, cur_pos, wt);
    if (bad_wt(wt))
      throw runtime_error("Invalid Clause (weight):\"" + ln + "\"\n");
    if (warn_wt(wt) && ++nwarns1 < 4)
      cerr << "Warning zero weight clause \"" << ln << "\"\n";
    return wt;
  } else
    return 1;
}

/*
Wcnf::llint Wcnf::get_wt(std::istringstream& ss) {
  llint wt{};
  if (isWcnf) {
    ss >> wt;
    if (!ss)
      throw runtime_error("Invalid Clause (weight):\"" + ss.str() + "\"\n");
    if (bad_wt(wt))
      throw runtime_error("Invalid Clause (weight):\"" + ss.str() + "\"\n");
    if (warn_wt(wt) && ++nwarns1 < 4)
      cerr << "Warning zero weight clause \"" << ss.str() << "\"\n";
  } else {
    wt = 1;
  }
  return wt;
  }*/

/*void Wcnf::get_lits(vector<llint>& lits, std::istringstream& ss) {
  llint lit;
  lits.clear();
  // in input every clause must start and end on one line!
  while (true) {
    ss >> lit;
    if (!ss) throw runtime_error("Invalid Clause :\"" + ss.str() + "\"\n");
    if (lit == 0) {
      ss >> std::ws;
      if (!ss.eof()) {
        if (++nwarns2 < 4)
          cerr << "Warning clause \"" << ss.str()
               << "\" contains extra characters after zero lit (ignored)\n";
      }
      break;
    } else {
      lits.push_back(lit);
      if (var(lit) > nvars) nvars = var(lit);
    }
  }
  }*/

void Wcnf::get_lits(vector<llint> &lits, const char *&cur_pos,
                    const string &ln) {
  llint lit;
  lits.clear();
  while (true) {
    parse_num(ln, cur_pos, lit);
    if (lit == 0)
      break;
    else {
      lits.push_back(lit);
      if (var(lit) > nvars)
        nvars = var(lit);
    }
  }
}

void Wcnf::parse_clause(string &ln) {
  auto cur_pos = ln.c_str();
  if (!seen_p_line) {
    // new no p_line format
    if (ln.front() == 'h' || ln.front() == 'H') {
      get_lits(input_cls, ++cur_pos, ln);
      update_stats(true, 0, input_cls);
      clauses.push_back(Clause{true, 0, std::move(input_cls)});
    } else {
      auto wt{get_wt(cur_pos, ln)};
      sum_soft_wts += wt;
      get_lits(input_cls, cur_pos, ln);
      update_stats(false, wt, input_cls);
      clauses.push_back(Clause{false, wt, std::move(input_cls)});
    }
  } else {
    // old p_line format
    auto wt{get_wt(cur_pos, ln)};
    if (is_soft(wt))
      sum_soft_wts += wt;
    get_lits(input_cls, cur_pos, ln);
    update_stats(is_hard(wt), wt, input_cls);
    clauses.push_back(Clause{is_hard(wt), wt, std::move(input_cls)});
  }
  seen_clause = true;
}

void Wcnf::update_stats(bool hard, llint wt, const vector<llint> &lits) {
  if (hard) {
    hard_lens.update(static_cast<llint>(lits.size()));
  } else {
    weights.update(wt);
    soft_lens.update(static_cast<llint>(lits.size()));
  }
  instance_empty = false;
}

void Wcnf::compute_pline() {
  ncls = static_cast<llint>(clauses.size());
  adjust_wts();
}

void Wcnf::adjust_wts() {
  // if !old_fmt don't adjust top or wts of hard clauses
  // Actually these are probably fine to modify as they
  // should be ignored by the rest of the code. But check
  // this anyway.

  // check if old weighted instances with top specified could have
  // some of its clauses hardened (a common case).  This requires
  // linking to a sat solver---and will reset sum_soft_wts
  if (wcnf_no_top)
    compute_blo_top(); //! old_fmt ==> !wcnf_no_top

  if (old_fmt)
    top = sum_soft_wts + 1;
  for (auto &c : clauses)
    if (c.hard) {
      if (old_fmt)
        c.wt = top;
    } else
      unique_wts.insert(c.wt);

  if (!preserve) {
    // Check for only one weight that is not equal to 1.
    // Reset that case to all softs having weight 1.
    if (unique_wts.size() == 1 && *unique_wts.begin() != 1) {
      sum_soft_wts = soft_lens.n;
      if (old_fmt)
        top = sum_soft_wts + 1;
      for (auto &c : clauses)
        if (c.hard) {
          if (old_fmt)
            c.wt = top;
        } else
          c.wt = 1;
      // also reset weight stats
      weights.total = sum_soft_wts;
      weights.max = 1;
      weights.min = 1;
      weights.ave = 1;
      weights.varN = 0;
    }
  }
}

void Wcnf::remap_vars() {
  if (!preserve) {
    vector<char> var_in_formula(static_cast<size_t>(nvars) + 1, 0);
    vector<llint> map(static_cast<size_t>(nvars) + 1, 0);
    for (auto &c : clauses)
      for (auto l : c.lits)
        var_in_formula[varIdx(l)] = 1;
    llint nxtvar{1};
    for (size_t i = 0; i < var_in_formula.size(); ++i)
      if (var_in_formula[i])
        map[i] = nxtvar++;
    for (auto &c : clauses)
      for (auto &l : c.lits)
        if (l < 0)
          l = -map[varIdx(l)];
        else
          l = map[varIdx(l)];
    nvars = nxtvar - 1;
  }
}

void Wcnf::outputInstance(ostream &os) const {
  auto comment{comment_lines.begin()};
  auto clause{clauses.begin()};
  bool p_print{false};
  for (auto t : line_type) {
    if (t == c_line)
      os << *comment++ << "\n";
    else if (t == p_line) {
      if (old_fmt) {
        os << "p wcnf " << nvars << " " << ncls << " " << top << "\n";
        p_print = true;
      }
    } else if (t == cls_line) {
      if (old_fmt && !p_print) {
        os << "p wcnf " << nvars << " " << ncls << " " << top << "\n";
        p_print = true;
      }
      if (clause->hard || (!clause->hard && clause->wt > 0))
        os << *clause << "\n";
      ++clause;
    }
  }
}

void Wcnf::outputCNF(ostream &os, bool hards, const string &fname) const {
  llint nvars_{}, ncls_{};
  for (auto &c : clauses) {
    if (hards && !c.hard)
      continue;
    ++ncls_;
    for (auto l : c.lits) {
      if (var(l) > nvars_)
        nvars_ = var(l);
    }
  }
  os << "c CNF from WCNF instance \"" << (fname.empty() ? "<stdin>" : fname)
     << "\"" << (hards ? " (hard clauses only)" : "") << '\n';

  os << "p cnf " << nvars_ << " " << ncls_ << '\n';
  for (auto &c : clauses) {
    if (hards && !c.hard)
      continue;
    for (auto l : c.lits)
      os << l << " ";
    os << 0 << "\n";
  }
}

void Wcnf::output_stat_lines(ostream &os) const {
  os << std::fixed << std::setprecision(4);
  os << "c Standarized MaxSat Instance\n"
     << "c{\n"
     << "c \"sha1sum\": \"" << sha1 << "\",\n"
     << "c \"nvars\": " << nvars << ",\n"
     << "c \"ncls\": " << ncls << ",\n"
     << "c \"total_lits\": " << hard_lens.total + soft_lens.total << ",\n"
     << "c \"nhards\": " << hard_lens.n << ",\n"
     << "c \"nhard_nlits\": " << hard_lens.total << ",\n"
     << "c \"nhard_len_stats\":\n"
     << "c    { \"min\": " << hard_lens.min << ",\n"
     << "c      \"max\": " << hard_lens.max << ",\n"
     << "c      \"ave\": " << hard_lens.ave << ",\n"
     << "c      \"stddev\": " << hard_lens.stdev() << " },\n"
     << "c \"nsofts\": " << soft_lens.n << ",\n"
     << "c \"nsoft_nlits\": " << soft_lens.total << ",\n"
     << "c \"nsoft_len_stats\":\n"
     << "c    { \"min\": " << soft_lens.min << ",\n"
     << "c      \"max\": " << soft_lens.max << ",\n"
     << "c      \"ave\": " << soft_lens.ave << ",\n"
     << "c      \"stddev\": " << soft_lens.stdev() << " },\n"
     << "c \"nsoft_wts\": " << unique_wts.size() << ",\n"
     << "c \"soft_wt_stats\":\n"
     << "c    { \"min\": " << weights.min << ",\n"
     << "c      \"max\": " << weights.max << ",\n"
     << "c      \"ave\": " << weights.ave << ",\n"
     << "c      \"stddev\": " << weights.stdev() << " }\n"
     << "c}\n"
     << "c------------------------------------------------------------\n";
}

ostream &operator<<(ostream &os, const Wcnf::Clause &c) {
  // nasty use of static variable...but so far can't find
  // cleaner way that is simple enough
  if (!Wcnf::old_fmt) {
    if (c.hard)
      os << "h ";
    else
      os << c.wt << ' ';
  } else {
    os << c.wt << ' ';
  }
  for (auto l : c.lits)
    os << l << ' ';
  os << "0";
  return os;
}

std::vector<Wcnf::llint> Wcnf::find_blo_wts() const {
  // find weights that are larger then the sum of the weight of
  // all clauses less then them.
  vector<llint> blo_wts;
  vector<llint> diffWts;
  vector<int> diffWtCounts;
  vector<llint> wts;
  for (auto &c : clauses)
    if (!c.hard)
      wts.push_back(c.wt);
  if (wts.empty())
    return blo_wts;

  std::sort(wts.begin(), wts.end());
  diffWts.push_back(wts[0]);
  diffWtCounts.push_back(1);
  for (size_t i = 1, j = 0; i < wts.size(); i++)
    if (wts[j] == wts[i])
      diffWtCounts.back()++;
    else {
      j = i;
      diffWts.push_back(wts[i]);
      diffWtCounts.push_back(1);
    }

  llint wtSoFar = diffWts[0] * diffWtCounts[0];
  for (size_t i = 1; i < diffWts.size(); i++) {
    if (diffWts[i] > wtSoFar)
      blo_wts.push_back(diffWts[i]);
    wtSoFar += diffWts[i] * diffWtCounts[i];
  }
  return blo_wts;
}

void Wcnf::compute_blo_top() {
  // Old instances had weighted instances with no hard clauses but for
  // which the highest weight soft clauses could soundly be hardned.
  // Check for this so that we can standardize these properly.
#ifdef BLO_TOP
  vector<llint> blo_wts{find_blo_wts()};
  if (blo_wts.empty())
    return;

  auto solver = std::make_unique<CaDiCaL::Solver>();
  solver->limit("conflicts", 10e6);
  for (auto &c : clauses)
    if (c.hard) {
      for (auto l : c.lits) {
        solver->add(static_cast<int>(l));
      }
      solver->add(0);
    }

  llint maxWt{weights.max + 1};
  llint maxHardenWt{weights.max + 1};
  for (auto p = blo_wts.crbegin(); p != blo_wts.crend(); p++) {
    for (auto &c : clauses) {
      if (!c.hard && c.wt >= *p && c.wt < maxWt) {
        for (auto l : c.lits) {
          solver->add(static_cast<int>(l));
        }
        solver->add(0);
      }
    }
    maxWt = *p;
    bool canHarden = (solver->solve() == 10);
    if (canHarden) {
      maxHardenWt = *p;
    } else
      break;
  }
  if (maxHardenWt <= weights.max) { // can change some softs to hards
    cerr << "Warning: Hardening some soft clauses.\n";

    sum_soft_wts = 0;
    for (auto &c : clauses) {
      if (c.hard)
        continue;
      else if (c.wt >= maxHardenWt)
        c.hard = true;
      else
        sum_soft_wts += c.wt;
    }
  }
#endif
}

int64_t Wcnf::get_model_cost(const vector<int8_t> &model,
                             std::set<size_t> &unsat_softs) const {
  // takes advantage of the observation that most maxsat instances
  // have solutions falsifying a relatively small number of softs.
  // store in a set for easy to compare different unsat sets (efficient
  // enough for this context)

  if (static_cast<int64_t>(model.size()) != nvars + 1)
    return -2;
  int64_t false_wt{};
  unsat_softs.clear();
  size_t cls_idx{0};
  for (auto &c : clauses) {
    bool sat = false;
    for (auto l : c.lits) {
      int tv = model[varIdx(l)];
      if (tv < -1 || tv > 1)
        return -2;
      if ((tv == -1 && l < 0) || (tv == 1 && l > 0)) {
        sat = true;
        break;
      }
    }
    if (!sat) {
      if (c.hard)
        return -1;
      else {
        false_wt += c.wt;
        unsat_softs.insert(cls_idx);
      }
    }
    ++cls_idx;
  }
  return false_wt;
}
