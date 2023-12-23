defmodule Brainfuck do
   @moduledoc """
  Brainfuck interpreter.
  """
  # increment the value at the current memory address.
  @m_inc "+"

  # decrement the value at the current memory address.
  @m_dec "-"

  # move to the next memory address.
  @mv_next ">"

  # move to the previous memory address.
  @mv_prev "<"

  # output the byte at the current memory address as a character.
  @output_b_char "."

  # input a byte and store it at the current memory address.

  @input_b_char ","

  # begin a loop if the value at the current memory address is zero.
  @loop_begin "["

  # end the current loop if the value at the current memory address is nonzero.
  @loop_end "]"

  # empty initial memory state.
  @empty ""

  @spec run(binary) :: {integer, list, bitstring}
  def run(program), do: run(program, 0, [0], @empty)

  # final condition
  defp run(@empty, addr, mem, output), do: {addr, mem, output}

  # runners
  defp run(@m_inc <> rest, addr, mem, output) do
    run(rest, addr, mem |> func_inc_at(addr), output)
  end

  defp run(@m_dec <> rest, addr, mem, output) do
    run(rest, addr, mem |> func_dec_at(addr), output)
  end

  defp run(@mv_next <> rest, addr, mem, output) when addr + 1 == length(mem) do
    run(rest, addr + 1, mem ++ [0], output)
  end

  defp run(@mv_next <> rest, addr, mem, output) do
    run(rest, addr + 1, mem, output)
  end

  defp run(@mv_prev <> rest, addr, mem, output) when addr == 0 do
    run(rest, 0, [0] ++ mem, output)
  end

  defp run(@mv_prev <> rest, addr, mem, output) do
    run(rest, addr - 1, mem, output)
  end

  defp run(@output_b_char <> rest, addr, mem, output) do
    run(rest, addr, mem, output <> (mem |> func_char_at(addr)))
  end

  defp run(@input_b_char <> rest, addr, mem, output) do
    val =
      case IO.getn("Input\n", 1) do
        :eof -> 0
        c -> c |> to_charlist |> Enum.at(0)
      end

    run(rest, addr, mem |> func_put_at(addr, val), output)
  end

  defp run(@loop_begin <> rest, addr, mem, output) do
    case mem |> func_byte_at(addr) do
      0 ->
        run(rest |> func_goto_loop_end, addr, mem, output)

      _ ->
        {a, m, o} = run(rest |> func_loop_body, addr, mem, output)
        run(@loop_begin <> rest, a, m, o)
    end
  end

  # drops every other character
  defp run(<<_>> <> rest, addr, mem, output), do: run(rest, addr, mem, output)

  # helpers

  defp func_inc_at(list, addr), do: List.update_at(list, addr, &((&1 + 1) |> rem(255)))
  defp func_dec_at(list, addr), do: List.update_at(list, addr, &((&1 - 1) |> rem(255)))
  defp func_put_at(list, addr, val), do: List.replace_at(list, addr, val)

  defp func_byte_at(list, addr), do: list |> Enum.at(addr)
  defp func_char_at(list, addr), do: [list |> func_byte_at(addr)] |> to_string()

  defp func_find_matching_loop_end(source), do: func_find_matching_loop_end(source, 1, 0)
  defp func_find_matching_loop_end(_, 0, acc), do: acc
  defp func_find_matching_loop_end(@empty, _, _), do: raise("unbalanced loop")
  defp func_find_matching_loop_end(@loop_begin <> rest, depth, acc), do: func_find_matching_loop_end(rest, depth + 1, acc + 1)
  defp func_find_matching_loop_end(@loop_end <> rest, depth, acc), do: func_find_matching_loop_end(rest, depth - 1, acc + 1)
  defp func_find_matching_loop_end(<<_>> <> rest, depth, acc), do: func_find_matching_loop_end(rest, depth, acc + 1)

  defp func_goto_loop_end(source), do: source |> String.slice((source |> func_find_matching_loop_end)..-1)
  defp func_loop_body(source), do: source |> String.slice(0..((source |> func_find_matching_loop_end) - 1))
end
