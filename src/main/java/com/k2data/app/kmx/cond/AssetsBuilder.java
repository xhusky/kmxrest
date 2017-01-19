package com.k2data.app.kmx.cond;

import com.k2data.app.kmx.KmxInitParams;
import com.k2data.app.kmx.domain.AssetsDomain;
import com.k2data.app.kmx.enums.KmxCondType;

import java.util.HashMap;
import java.util.Map;

/**
 * @author lidong 17-1-19.
 */
public class AssetsBuilder extends KmxCondBuilder {

    private KmxInitParams initParams;
    private Map<String, String> params = new HashMap<>();

    public AssetsBuilder(KmxInitParams initParams) {
        this.initParams = initParams;
    }

    @Override
    public KmxCond build() {
        KmxCond kmxCond = new KmxCond();
        kmxCond.setUrl(initParams.getUrls().get(KmxCondType.assets));
        kmxCond.setParams(params);
        kmxCond.setClazz(AssetsDomain.class);

        return kmxCond;
    }

    public AssetsBuilder addParams(String key, String value) {
        params.put(key, value);
        return this;
    }

}
