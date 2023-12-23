defmodule Brainfuck do
   @moduledoc """
  Brainfuck interpreter in Elixir.
  """

  @doc """
  Increment the value at the current memory address.
  """
  @m_inc "+"

  @doc """
  Decrement the value at the current memory address.
  """
  @m_dec "-"

  @doc """
  Move to the next memory address.
  """
  @mv_next ">"

  @doc """
  Move to the previous memory address.
  """
  @mv_prev "<"

  @doc """
  Output the byte at the current memory address as a character.
  """
  @output_b_char "."

  @doc """
  Input a byte and store it at the current memory address.
  """
  @input_b_char ","

  @doc """
  Begin a loop if the value at the current memory address is zero.
  """
  @loop_begin "["

  @doc """
  End the current loop if the value at the current memory address is nonzero.
  """
  @loop_end "]"

  @doc """
  Empty initial memory state.
  """
  @empty ""

  @spec run(binary) :: {integer, list, bitstring}
  def run(program), do: run(program, 0, [0], @empty)

  # final condition
  defp run(@empty, addr, mem, output), do: {addr, mem, output}

  # commands
  defp run(@m_inc <> rest, addr, mem, output) do
    run(rest, addr, mem |> inc_at(addr), output)
  end

  defp run(@m_dec <> rest, addr, mem, output) do
    run(rest, addr, mem |> dec_at(addr), output)
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
    run(rest, addr, mem, output <> (mem |> char_at(addr)))
  end

  defp run(@input_b_char <> rest, addr, mem, output) do
    val =
      case IO.getn("Input\n", 1) do
        :eof -> 0
        c -> c |> to_charlist |> Enum.at(0)
      end

    run(rest, addr, mem |> put_at(addr, val), output)
  end

  defp run(@loop_begin <> rest, addr, mem, output) do
    case mem |> byte_at(addr) do
      0 ->
        run(rest |> jump_to_lend, addr, mem, output)

      _ ->
        {a, m, o} = run(rest |> loop_body, addr, mem, output)
        run(@loop_begin <> rest, a, m, o)
    end
  end

  # drop every other character
  defp run(<<_>> <> rest, addr, mem, output), do: run(rest, addr, mem, output)

  # helpers
  defp inc_at(list, addr), do: List.update_at(list, addr, &((&1 + 1) |> rem(255)))
  defp dec_at(list, addr), do: List.update_at(list, addr, &((&1 - 1) |> rem(255)))
  defp put_at(list, addr, val), do: List.replace_at(list, addr, val)

  defp byte_at(list, addr), do: list |> Enum.at(addr)
  defp char_at(list, addr), do: [list |> byte_at(addr)] |> to_string()

  defp match_lend(source), do: match_lend(source, 1, 0)
  defp match_lend(_, 0, acc), do: acc
  defp match_lend(@empty, _, _), do: raise("unbalanced loop")
  defp match_lend(@loop_begin <> rest, depth, acc), do: match_lend(rest, depth + 1, acc + 1)
  defp match_lend(@loop_end <> rest, depth, acc), do: match_lend(rest, depth - 1, acc + 1)
  defp match_lend(<<_>> <> rest, depth, acc), do: match_lend(rest, depth, acc + 1)

  defp jump_to_lend(source), do: source |> String.slice((source |> match_lend)..-1)
  defp loop_body(source), do: source |> String.slice(0..((source |> match_lend) - 1))
end
