package com.k2data.app.kmx.cond;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.k2data.app.kmx.KmxInitParams;
import com.k2data.app.kmx.enums.KmxCondType;
import com.k2data.app.kmx.enums.RequestType;
import com.k2data.app.kmx.utils.JsonUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * fieldsGroup v2 查询条件 builder, 可链式调用添加条件, 最后调用 {@code build()} 生成查询条件
 *
 * @author lidong 17-1-19.
 */
public class PostDataBuilder extends KmxCondBuilder {

    private KmxInitParams initParams;

    public PostDataBuilder(KmxInitParams initParams) {
        this.initParams = initParams;
    }

    public PostBuilder post() {
        return new PostBuilder();
    }

    @JsonInclude(JsonInclude.Include.NON_NULL)
    public class PostBuilder {
        private String fieldGroupId;
        private Map<String, Long> sampleTime = new HashMap<>();
        private List<Map<String, Object>> fields = new ArrayList<>();

        public PostBuilder fieldGroupId(String fieldGroupId) {
            this.fieldGroupId = fieldGroupId;
            return this;
        }
        public PostBuilder timestamp(Long timestamp) {
            this.sampleTime.put("timestamp", timestamp);
            return this;
        }
        public PostBuilder addField(String field, Object value) {
            return addField(field, value, false);
        }
        public PostBuilder addField(String field, Object value, boolean isIdField) {
            Map<String, Object> fieldMap = new HashMap<>();
            fieldMap.put("fieldId", field);
            fieldMap.put("fieldValue", value);
            if (isIdField) {
                fieldMap.put("isIdField", true);
            }

            this.fields.add(fieldMap);
            return this;
        }
        public KmxCond build() {
            Map<String, String> params = new HashMap<>();
            params.put("query", JsonUtils.toJsonString(this));

            KmxCond kmxCond = new KmxCond();
            kmxCond.setUrl(initParams.getUrls().get(KmxCondType.postData));
            kmxCond.setParams(params);
            kmxCond.setClazz(String.class);
            kmxCond.setRequestType(RequestType.POST);
            kmxCond.setHandleResponse(false);

            return kmxCond;
        }
    }

}
