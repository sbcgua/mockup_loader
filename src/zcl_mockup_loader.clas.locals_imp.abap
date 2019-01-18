define transfer_setting.
* &1 component
* &2 name variable (character sequence)

  if l_variable-name = &2.
    l_settings-&1 = l_variable-low.
  endif.

end-of-definition.
