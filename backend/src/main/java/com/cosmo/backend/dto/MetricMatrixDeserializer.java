package com.cosmo.backend.dto;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.JsonNode;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * Deserializes the metric matrix from JSON so that each element can be either
 * a string (e.g. "1", "x(1)**2") or a number (e.g. 1, 0.5). Converts all to String
 * so the backend and Fortran layer receive a consistent type.
 */
public class MetricMatrixDeserializer extends JsonDeserializer<List<List<String>>> {

    @Override
    public List<List<String>> deserialize(JsonParser p, DeserializationContext ctxt) throws IOException {
        JsonNode root = p.getCodec().readTree(p);
        if (root == null || root.isNull()) {
            return null;
        }
        if (!root.isArray()) {
            throw new IllegalArgumentException("metric must be an array of rows");
        }
        List<List<String>> result = new ArrayList<>();
        for (JsonNode row : root) {
            if (row == null || row.isNull()) {
                result.add(new ArrayList<>());
                continue;
            }
            if (!row.isArray()) {
                throw new IllegalArgumentException("metric row must be an array");
            }
            List<String> rowList = new ArrayList<>();
            for (JsonNode cell : row) {
                if (cell == null || cell.isNull()) {
                    rowList.add("0");
                    continue;
                }
                if (cell.isTextual()) {
                    rowList.add(cell.asText());
                } else if (cell.isNumber()) {
                    rowList.add(cell.asText());
                } else if (cell.isBoolean()) {
                    rowList.add(cell.asBoolean() ? "1" : "0");
                } else {
                    rowList.add(cell.asText());
                }
            }
            result.add(rowList);
        }
        return result;
    }
}
