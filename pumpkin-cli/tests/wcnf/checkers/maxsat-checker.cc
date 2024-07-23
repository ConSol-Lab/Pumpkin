/***********[verify_soln.cc]
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

/**
 * Modifications:
 *
 * Remove the decompression wrapper. For tests we don't gzip any
 * instances/outputs. This makes it easier to build cross-platform.
 *
 * Renamed "verify_soln" to "maxsat-checker" to distinguish from other types of
 * solution checkers.
 */

#include <cctype>
#include <fstream>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>

#include "Wcnf.h"
#include "stream_utils.h"

using std::cerr;
using std::cin;
using std::cout;
using std::string;
using std::vector;

using llint = int64_t;

void print_usage_short() {
  // clang-format off
  cout << "USAGE: maxsat-checker"
       << " [-h|-help|--help|-help-topk] [-mlv] [-v] [-e] [-s] [-topk] [-o <OPTIMUMWT>] wcnf_file [soln_file]\n"
       << "   Read a Maxsat \"wcnf_file\" and verify each v-line in \"soln_file\" (or read from stdin\n"
       << "   if no \"soln_file\" specified)\n";
  // clang-format on
}

void print_usage() {
  print_usage_short();
  cout
      // clang-format off
      << " INPUT:\n"
      << "   (a) wcnf_file---a maxsat instance in any format used in the maxsat evaluations\n"
      << "       (old or new)\n"
      << "   (b) soln_file or <stdin> output of a maxsat solver outputting o-lines and v-lines\n"
      << "       to specify the solutions to wcnf_file it finds.\n"
      << "       An o-line specifying the cost of the solution is expected in the solver output\n"
      << "       before each v-line which specifies the solution.\n"
      << "\n"
      << "    * Each v-line is matched with the most recent previous o-line.\n"
      << "    * Other lines and extra o-lines are ignored (or echoed if -e is used).\n"
      << "    * v-lines can be in either the new or the old maxsat evaluation format.\n"
      << "      o-lines are in the maxsat evaluation format.\n"
      << " OUTPUT:\n"
      << "    For each processed v-line(s) in input, output the line\n"
      << "c <WT> [VERIFIED [NOT_MATCHED]|INVALID]\n"
      << "    VERIFIED                = solution is feasible\n"
      << "    VERIFIED NOT_MATCHED    = solution is feasible but cost does not match previous o-line\n"
      << "    INVALID                 = solution is not feasible (does not satisfy the hards)\n"
      << "\n"
      << "    If input contains an \"s OPTIMUM\" line, output the line\n"
      << "c <WT> [CLAIMED OPTIMUM|VALID OPTIMUM|INVALID OPTIMUM]\n"
      << "    where <WT> is the cost of the best solution contained in input.\n"
      << "    CLAIMED OPTIMUM = solution is valid and has cost <WT> (but optimality cannot be\n"
      << "                      determined)\n"
      << "    VALID OPTIMUM   = solution is valid and has cost = supplied optimum\n"
      << "    INVALID OPTIMUM = solution is valid and but has cost > supplied optimum\n"
      << "\n"
      << "   If the input contains any verified v-lines finally print the line\n"
      << "c <WT> BEST VERIFIED SOLUTION\n"
      << "   Note: that <WT> is the best solution cost computed by the verifier not the\n"
      << "         the best given on the o-lines\n"
      << "\n"
      << " TOPK: verifying output of the 2020 topk track can be accomplished with -topk\n"
      << "      use help-topk for more information about topk verification\n"
      << " OPTIONS:\n"
      << "  -mlv  (multi line v-lines) allow the v-line to be specified across multiple\n"
      << "        consecutive lines.\n"
      << "        Each line in this group of v-lines must starts with \"v \" and the\n"
      << "        lines must appear right after each other in the input\n"
      << "        Warning: with -mlv ensure a non-v-line appears between the v-lines specifying\n"
      << "        different solutions\n"
      << "  -v    (echo v-lines) output the v-lines as well as the verification lines above\n"
      << "  -e    (echo) echo all lines to the output (verification lines above are added).\n"
      << "  -o    <OPTIMUMWT> a supplied OPTIMUMWT used to verify \"s OPTIMUM\" lines\n"
      << "  -s    (server input) is specified then STAREXEC's prefix time stamp is removed\n"
      << "        from the input lines\n"
      << "  -topk verify topk solver output\n"
      << "  -mlv, -v, -e, and -s all default to false\n"
      << "\n"
      << " Return codes:\n"
      << "   0 if all solutions are verified, 1 if any solution pair or any \"s OPTIMUM\" claim\n"
      << "   is invalid.\n";
  // clang-format on
}

void print_usage_topk() {
  print_usage_short();
  // clang-format off
  cout
    << " TOPK:\n"
    << "   check if solver v-lines are (a) correct, (b) have non-decreasing costs and (c) \n"
    << "   falsify different different sets of soft clauses (as required by topk track)\n"
    << "\n"
    << "    if solutions specified in output by the v-lines satisfy (a)-(c) above then output\n"
    << "    the line\n"
    << "c <K> TOPK [VERIFIED|INVALID]\n"
    << "    where <K> is the number of valid v-lines in input\n"
    << "\n"
    << "    The solver can claim that no further topk solutions exist by outputting an empty v-line\n"
    << "    If such is found in the solver output, then output the line\n"
    << "c TOPK ALLSOLUTIONS CLAIMED (<#solns>)\n"
    << "where <#soln> is the number of topk solutions found in the solver output\n";
  // clang-format on
}

llint parse_o_line(const string &o_line);
llint check_v_line(const Wcnf &theFormula, string &vline, llint oline_wt,
                   bool topk, vector<std::set<size_t>> &unsat_softs);
void remove_server_prefix(string &ln);
void remove_trailing_ws(string &ln);

constexpr llint BAD_WT{-1};
constexpr llint NO_MORE_SOLNS{-2};

bool process_solns(const Wcnf &theFormula, std::istream &soln_stream,
                   bool multi_line_v, bool echo_vline, bool echo,
                   bool server_input, bool topk, llint optimum) {
  bool error{false}, no_more_solns_claimed{false}, optimum_claimed{false};
  llint oline_wt{BAD_WT}, best_soln_wt{BAD_WT};
  string ln;
  vector<std::set<size_t>> unsat_softs;
  vector<llint> soln_weights;

  while (std::getline(soln_stream, ln)) {
    if (server_input)
      remove_server_prefix(ln);
    if (echo)
      cout << ln << '\n';

    if (ln.front() == 'o' || ln.front() == 'O') {
      if ((oline_wt = parse_o_line(ln)) == BAD_WT)
        cout << "c bad o-line detected (ignoring) " << ln.substr(0, 40) << "\n";
    } else if (ln.front() == 's' || ln.front() == 'S') {
      if (ln.find("OPTIMUM FOUND") != string::npos ||
          ln.find("optimum found") != string::npos)
        optimum_claimed = true;
    } else if (ln.front() == 'v' || ln.front() == 'V') {
      if (!echo && echo_vline)
        cout << ln << '\n';
      llint soln_wt;
      no_more_solns_claimed = false;
      string vline{ln, 2};
      if (multi_line_v) {
        while (soln_stream.peek() == 'v' || soln_stream.peek() == 'V') {
          std::getline(soln_stream, ln);
          if (!echo && echo_vline)
            cout << ln << '\n';
          vline.append(ln, 2);
        }
      }
      soln_wt = check_v_line(theFormula, vline, oline_wt, topk, unsat_softs);
      if (soln_wt == NO_MORE_SOLNS)
        no_more_solns_claimed = true;
      else if (soln_wt == BAD_WT)
        error = true;
      else {
        if (best_soln_wt == BAD_WT || soln_wt < best_soln_wt)
          best_soln_wt = soln_wt;
        soln_weights.push_back(soln_wt);
      }
    }
  }

  if (optimum_claimed && best_soln_wt != BAD_WT) {
    if (optimum >= 0) {
      if (best_soln_wt <= optimum)
        cout << "c " << best_soln_wt << " VALID OPTIMUM\n";
      if (best_soln_wt < optimum)
        cout << "c WARNING: supplied optimum (" << optimum << ") invalid "
             << best_soln_wt << " is a better solution\n";
      if (best_soln_wt > optimum) {
        error = true;
        cout << "c " << best_soln_wt << " INVALID OPTIMUM\n";
      }
    } else
      cout << "c " << best_soln_wt << " CLAIMED OPTIMUM\n";
  }

  if (topk) {
    bool non_decreasing = true;
    bool all_diff = true;
    for (size_t i = 1; i < soln_weights.size(); ++i)
      if (soln_weights[i] < soln_weights[i - 1])
        non_decreasing = false;
    for (size_t i = 0; all_diff && i < unsat_softs.size(); ++i)
      for (size_t j = i + 1; all_diff && j < unsat_softs.size(); ++j)
        if (unsat_softs[i] == unsat_softs[j])
          all_diff = false;

    if (all_diff && non_decreasing) {
      cout << "c " << unsat_softs.size() << " TOPK VERIFIED\n";
      if (no_more_solns_claimed)
        cout << "c TOPK ALLSOLUTIONS CLAIMED (" << unsat_softs.size() << ")\n";
    } else {
      cout << "c " << unsat_softs.size() << " TOPK INVALID ";
      if (!non_decreasing)
        cout << "(solution costs decreasing)\n";
      if (!all_diff)
        cout << "(some solutions satisfy same softs)\n";
    }
  }

  return !error;
}

llint parse_o_line(const string &ln) {
  if (ln.size() <= 2 || ln[1] != ' ')
    return BAD_WT;
  std::istringstream ss(ln);
  ss.get(), ss.get();
  llint wt;
  ss >> wt;
  if (!ss)
    return BAD_WT;
  if (wt < 0)
    return BAD_WT;
  return wt;
}

llint check_v_line(const Wcnf &theFormula, string &vline, llint oline_wt,
                   bool topk, vector<std::set<size_t>> &unsat_softs) {
  // cout << vline << "\n";
  remove_trailing_ws(vline);
  char mode{'u'};
  if (vline.empty())
    mode = 'e';
  else if (std::all_of(vline.begin(), vline.end(), [](char x) {
             return (std::isspace(x) || x == '1' || x == '0');
           }))
    mode = 'n';
  else if (std::all_of(vline.begin(), vline.end(), [](char x) {
             return (std::isspace(x) || std::isdigit(x) || x == '-');
           }))
    mode = 'o';

  if (mode == 'e')
    return NO_MORE_SOLNS;
  if (mode == 'u') {
    cout << "c unparsable vline detected" << vline.substr(0, 40) << "\n";
    return BAD_WT;
  }

  // vars start at 1---so model[0] is not used.
  vector<int8_t> model(static_cast<size_t>(theFormula.maxVar() + 1), 0);

  // init the model
  if (mode == 'n') {
    // new format. vlines that are too short are ok. Just check if the
    // literals that are set are sufficient to satisfy the formula. Note
    // that clauses with all unset literals will be regarded to be unsat.
    vline.erase(std::remove_if(vline.begin(), vline.end(),
                               [](char x) { return std::isspace(x); }),
                vline.end());
    for (size_t i = 0; i < vline.size() && i + 1 < model.size(); ++i) {
      if (vline[i] == '1')
        model[i + 1] = 1;
      else if (vline[i] == '0')
        model[i + 1] = -1;
    }
    if (vline.size() + 1 != model.size())
      cout << "c WARNING. vline not same size as number of variables\n";
  } else if (mode == 'o') {
    std::stringstream ss(vline);
    llint lit;
    while (ss >> lit) {
      if (std::llabs(lit) > theFormula.maxVar() || lit == 0) {
        cout << "c bad vline detected, lit " << lit << " not in formula\n";
        return BAD_WT;
      }
      model[static_cast<size_t>(std::labs(lit))] = lit < 0 ? -1 : 1;
    }
  }

  // check the model
  std::set<size_t> model_unsat_softs;
  auto w = theFormula.get_model_cost(model, model_unsat_softs);
  if (w > 0) {
    cout << "c " << w << " VERIFIED";
    if (w != oline_wt) {
      cout << " NOT_MATCHED";

      // If the reported cost does not match the evaluated cost, make sure to
      // exit with a non-zero exit code.
      w = BAD_WT;
    }
    cout << "\n";
    if (topk)
      unsat_softs.push_back(std::move(model_unsat_softs));
  } else if (w == -1) {
    cout << "c " << 0 << " INVALID (solution not feasible)\n";
  } else if (w == -2)
    cout << "c " << 0 << " ERROR---Internal error\n";
  return w;
}

void remove_server_prefix(string &ln) {
  ln.erase(0, ln.find_first_not_of(".0123456789/\t "));
}

void remove_trailing_ws(string &ln) {
  while (std::isspace(ln.back()))
    ln.pop_back();
}

int main(int argc, char *argv[]) {
  /* Process the solution lines provided and check that they achieve
     the specified cost of the wcnf instance.
   */
  std::ios::sync_with_stdio(false);
  cin.tie(0);
  cout.tie(0);
  cerr.tie(0);

  string wcnf_file, soln_file;
  bool multi_line_v{false}, echo_vline{false}, echo{false}, topk{false},
      server_input{false};
  llint optimum{BAD_WT};

  try {
    for (int i = 1; i < argc; ++i) {
      string arg;
      arg = argv[i];
      if (arg == "-help" || arg == "-h" || arg == "--help") {
        if (arg == "-h")
          print_usage_short();
        else
          print_usage();
        exit(1);
      } else if (arg == "-help-topk" || arg == "--help-topk") {
        print_usage_topk();
        exit(1);
      } else if (arg == "-v")
        echo_vline = true;
      else if (arg == "-mlv")
        multi_line_v = true;
      else if (arg == "-e")
        echo = true;
      else if (arg == "-s")
        server_input = true;
      else if (arg == "-topk")
        topk = true;
      else if (arg == "-o") {
        if (i + 1 >= argc)
          throw std::runtime_error("-o not provided with argument \"" + arg +
                                   "\"\n");
        string opt{argv[++i]};
        try {
          optimum = stoll(opt);
        } catch (const std::invalid_argument &a) {
          throw std::runtime_error("-o not provided with invalid argument \"" +
                                   opt + "\"\n");
        } catch (const std::out_of_range &r) {
          throw std::runtime_error("-o with too large an argument \"" + opt +
                                   "\"\n");
        }
        if (optimum < 0)
          throw std::runtime_error(
              "-o provided with illegal value (must be >= 0)");
      } else if (arg[0] == '-')
        throw std::runtime_error("Unknown argument \"" + arg + "\"\n");
      else if (wcnf_file.empty())
        wcnf_file = arg;
      else {
        if (!soln_file.empty())
          throw std::runtime_error("extranous argument \"" + arg + "\"\n");
        soln_file = arg;
      }
    }
  } catch (std::runtime_error &ex) {
    cerr << "Error: bad arguments " << ex.what();
    exit(1);
  } catch (...) {
    exit(1);
  }

  std::ifstream wcnf_stream;
  wcnf_stream.open(wcnf_file, std::ios::in | std::ios::binary);
  if (!wcnf_stream.is_open()) {
    cerr << "Error: could not open file \"" << wcnf_file << "\"\n";
    exit(1);
  }

  std::ifstream soln_stream;
  if (!soln_file.empty()) {
    soln_stream.open(soln_file, std::ios::in | std::ios::binary);
    if (!soln_stream.is_open()) {
      cerr << "Error: could not open file \"" << soln_file << "\"\n";
      exit(1);
    }
  }

  bool no_errors{true};
  Wcnf theFormula;
  try {
    theFormula.loadFromStream(wcnf_stream);
  } catch (std::runtime_error &ex) {
    cerr << "Error Reading WCNF file: " << ex.what() << "\n";
    return 1;
  } catch (...) {
    cerr << "Error Reading WCNF file: unknown";
    return 1;
  }

  try {
    wcnf_stream.close();
    no_errors = process_solns(theFormula, soln_file.empty() ? cin : soln_stream,
                              multi_line_v, echo_vline, echo, server_input,
                              topk, optimum);
  } catch (std::runtime_error &ex) {
    cerr << "Error in solution stream: " << ex.what() << "\n";
    return 1;
  } catch (...) {
    cerr << "Error in solution stream: unknown";
    return 1;
  }
  return no_errors ? 0 : 1;
}
