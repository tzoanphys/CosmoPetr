package com.cosmo.backend.dto;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.JsonNode;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

/**
 * Deserializes parameterValues from JSON so that each value is always a Double.
 * Accepts both number (e.g. 0.002) and string (e.g. "0.002") to avoid loss when
 * clients or Jackson send numbers in different formats.
 */
public class ParameterValuesDeserializer extends JsonDeserializer<Map<String, Double>> {

    @Override
    public Map<String, Double> deserialize(JsonParser p, DeserializationContext ctxt) throws IOException {
        JsonNode root = p.getCodec().readTree(p);
        if (root == null || root.isNull()) {
            return null;
        }
        if (!root.isObject()) {
            return null;
        }
        Map<String, Double> result = new HashMap<>();
        root.fields().forEachRemaining(entry -> {
            String key = entry.getKey();
            JsonNode valueNode = entry.getValue();
            if (valueNode == null || valueNode.isNull()) {
                return;
            }
            double val;
            if (valueNode.isNumber()) {
                val = valueNode.asDouble();
            } else if (valueNode.isTextual()) {
                try {
                    val = Double.parseDouble(valueNode.asText().trim());
                } catch (NumberFormatException e) {
                    return;
                }
            } else {
                return;
            }
            result.put(key, val);
        });
        return result;
    }
}
