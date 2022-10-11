#include <chrono>
#include <iostream>
#include <mutex>
#include <thread>
#include <vector>

#include <lyra/lyra.hpp>

std::mutex m_;
std::vector<size_t> result_;

void check(size_t start, size_t finish)
{
  while (start < finish) {
    if (start % 11 == 0) {
      std::lock_guard l {m_};
      result_.push_back(start);
    }
    ++start;
  }
}

int main(int argc, char* argv[])
{
  int threads = 0;
  size_t limit = 0;
  auto cli = lyra::cli()
      | lyra::opt(threads, "threads")["-t"]["--threads"](
            "The number of threads for filler")
            .required()
      | lyra::opt(limit, "container size")["-n"]["--num"](
            "The size of resulting container")
            .required();

  auto result = cli.parse({argc, argv});
  if (!result) {
    std::cerr << "Error in command line: " << result.message() << std::endl;
    exit(1);
  }

  result_.reserve(limit);

  std::vector<std::thread> ths;
  ths.reserve(threads);

  std::cout << "start" << std::endl;
  const auto point1 = std::chrono::steady_clock::now();

  size_t start = 1;
  for (size_t i = 0; i < threads; i++) {
    ths.emplace_back(std::thread(check, start, std::min(start + limit / threads, limit)));
    start += limit / threads;
  }

  for (auto& th : ths) {
    th.join();
  }

  const auto duration = std::chrono::steady_clock::now() - point1;
  std::cout << "duration " << duration.count() << std::endl;

  return 0;
}
