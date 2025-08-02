#pragma once

#include <string>

namespace sysy::base {

class TreeDumper {
 public:
  const std::string& str() const { return result_; }

 protected:
  class PrefixWriterScope {
   public:
    explicit PrefixWriterScope(TreeDumper& dumper) : dumper_(dumper) {
      dumper_.Write('\n');

      dumper_.Write(dumper_.prefix_);
      dumper_.Write('|');
      dumper_.Write('-');

      dumper_.prefix_.push_back('|');
      dumper_.prefix_.push_back(' ');
    }

    ~PrefixWriterScope() {
      // Restore to the old prefix.
      dumper_.prefix_.resize(dumper_.prefix_.size() - 2);
    }

   private:
    TreeDumper& dumper_;
  };

  void Write(const std::string& line) { result_.append(line); }

  void Write(char c) { result_.push_back(c); }

 private:
  std::string result_;
  std::string prefix_;
};

}  // namespace sysy::base
