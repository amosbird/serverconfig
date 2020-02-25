local function do_capital_space(s)
   return string.upper(s .. " ")
end

function myproc(key, env)
   local engine = env.engine
   local context = engine.context

   --- accept: Control+v
   if (key:repr() == "Control+v") then
      --- when: composing
      if (context:is_composing()) then
         local s_orig = context.input
         --- do_capital_space
         local s = do_capital_space(s_orig)
         engine:commit_text(s)
         context:clear()
         return 1 -- kAccepted
      end
   end
   return 2 -- kNoop
end

return myproc
