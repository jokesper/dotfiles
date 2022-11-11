string.split = function(s,del)
    local toCut = {[0]={[2]=1}}
    for a,b in s
        :gmatch(('()%s()')
            :format(del
                :gsub('%W', '%%%0'))) do
        table.insert(toCut, {a-1,b})
    end
    table.insert(toCut, {#s})

    local slices = {}
    for i,cut in ipairs(toCut) do
        local slice = s:sub(toCut[i-1][2],cut[1])
        table.insert(slices, slice)
    end

    return slices
end
