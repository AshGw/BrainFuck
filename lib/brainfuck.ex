defmodule Brainfux do
  # Brainfuck ops
  @incr_val "+"
  @decr_val "-"
  @incr_pointer ">"
  @decr_pointer "<"
  @output_char "."
  @input_char ","
  @loop_start "["
  @loop_end "]"
  @empty_program ""

  # specs for the run function.
  @spec run(binary) :: {integer, list, bitstring}
  def run(program), do: run(program, 0, [0], @empty_program)

  # when the program is empty, return the current state.
  defp run(@empty_program, addr, mem, output), do: {addr, mem, output}

  # Increment the value
  defp run(@incr_val <> rest, addr, mem, output) do
    run(rest, addr, inc_at(mem, addr), output)
  end

  # decrement the value
  defp run(@decr_val <> rest, addr, mem, output) do
    run(rest, addr, dec_at(mem, addr), output)
  end

  # increment pointer operation with boundary check
  defp run(@incr_pointer <> rest, addr, mem, output) when addr + 1 == Kernel.map_size(mem) do
    run(rest, addr + 1, Map.put_new(mem, addr + 1, 0), output)
  end

   # decrement pointer operation with boundary check
   defp run(@decr_pointer <> rest, addr, mem, output) when addr == 0 do
    run(rest, 0, Map.put_new(mem, 0, 0), output)
  end


  # increment the pointer
  defp run(@incr_pointer <> rest, addr, mem, output) do
    run(rest, addr + 1, mem, output)
  end

  # decrement the pointer
  defp run(@decr_pointer <> rest, addr, mem, output) do
    run(rest, addr - 1, mem, output)
  end

  # output character operation
  defp run(@output_char <> rest, addr, mem, output) do
    run(rest, addr, mem, output <> Map.get(mem, addr) |> byte_to_string())
  end

  # input character operation
  defp run(@input_char <> rest, addr, mem, output) do
    case IO.getn("Input\n", 1) do
      <<c::binary-size(1)>> ->
        val = hd(to_charlist(c))
        run(rest, addr, Map.put_new(mem, addr, val), output)

      :eof ->
        run(rest, addr, Map.put_new(mem, addr, 0), output)
    end
  end

  # Loop start operation
  defp run(@loop_start <> rest, addr, mem, output) do
    case Map.get(mem, addr) do
      0 -> run(rest |> jump_to_loop_end, addr, mem, output)
      _ -> {a, m, o} = run(rest |> loop_body, addr, mem, output)
            run(@loop_start <> rest, a, m, o)
    end
  end

  # Drop every other character
  defp run(<<_>> <> rest, addr, mem, output), do: run(rest, addr, mem, output)

  # Base helpers
  defp inc_at(mem, addr), do: Kernel.defp(mem, addr, &(&1 + 1 |> rem(255)))
  defp dec_at(mem, addr), do: Kernel.defp(mem, addr, &(&1 - 1 |> rem(255)))

  # Byte-related helpers
  defp byte_to_string(byte), do: [byte] |> to_string()

  # Other helpers
  defp jump_to_loop_end(source), do: source |> String.slice((source |> find_matching_loop_end())..-1)
  defp loop_body(source), do: source |> String.slice(0..((source |> find_matching_loop_end()) - 1))

  # TODO:
  # Function to find the matching loop end
  defp find_matching_loop_end(@empty_program), do: {:error, "unbalanced loop"}
  defp find_matching_loop_end(source), do: find_matching_loop_end(source, 1, 0)
  defp find_matching_loop_end(_, 0, acc), do: acc
  defp find_matching_loop_end(@loop_start <> rest, depth, acc), do: find_matching_loop_end(rest, depth + 1, acc + 1)
  defp find_matching_loop_end(@loop_end <> rest, depth, acc), do: find_matching_loop_end(rest, depth - 1, acc + 1)
  defp find_matching_loop_end(<<_>> <> rest, depth, acc), do: find_matching_loop_end(rest, depth, acc + 1)
end
