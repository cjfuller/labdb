import React, { useCallback, useState } from "react";
import {
  AccessibleIcon,
  Button,
  Checkbox,
  Flex,
  IconButton,
  Popover,
  Strong,
  TextField,
} from "@radix-ui/themes";
import { MagnifyingGlassIcon } from "@radix-ui/react-icons";

import { Types, TypeKey } from "./search";
import { StyleSheet, css } from "aphrodite";

type Props = {
  doSearch: (
    terms: string,
    includeSeq: boolean,
    person: string | null,
    types: Set<TypeKey>
  ) => void;
};

function useStateWithEventSetter<T>(
  initialState: T,
  emptyStringIsNull: boolean = false
): [T, (e: object) => void] {
  const [value, setValue] = useState<T>(initialState);
  const eventSetter = useCallback(
    (e) =>
      setValue(
        emptyStringIsNull && e.target.value === "" ? null : e.target.value
      ),
    [setValue]
  );
  return [value, eventSetter];
}

function TypeSelector({
  setSelected,
  selected,
  name,
}: {
  setSelected: (selected: boolean) => void;
  selected: boolean;
  name: TypeKey;
}) {
  const onClick = useCallback(
    () => setSelected(!selected),
    [selected, setSelected]
  );
  return (
    <Button variant={selected ? "solid" : "outline"} onClick={onClick}>
      {name}
    </Button>
  );
}

function SearchPageContent(props: Props) {
  const [searchTerms, setSearchTerms] = useStateWithEventSetter<string>("");
  const [includeSequence, setIncludeSequence] = useState<boolean>(false);
  const [person, setPerson] = useStateWithEventSetter<string | null>(
    null,
    true
  );
  const [types, setTypes] = useState<Set<TypeKey>>(
    new Set(Object.keys(Types) as TypeKey[])
  );
  const sequenceSetterFromEvent = useCallback(
    (e: boolean | "indeterminate") => setIncludeSequence(e === true),
    [setIncludeSequence]
  );
  return (
    <Flex direction="column" gap="4">
      <TextField.Root>
        <TextField.Slot>
          <MagnifyingGlassIcon />
        </TextField.Slot>
        <TextField.Input
          placeholder="Search..."
          onChange={setSearchTerms}
          value={searchTerms}
        />
      </TextField.Root>
      <Flex direction="row" gap="2" align="center">
        <Checkbox
          checked={includeSequence}
          onCheckedChange={sequenceSetterFromEvent}
        />
        Include sequence in search?
      </Flex>
      <Flex direction="column" gap="0">
        <Strong>Filters</Strong>
        <div className={css(styles.filterDiv)}>
          <Flex direction="column" gap="4">
            <label style={{ marginTop: "4px" }}>
              Person
              <TextField.Input onChange={setPerson} value={person ?? ""} />
            </label>
            <Flex direction="column" gap="1">
              <div>Types to search</div>
              <Flex direction="row" gap="1" align="center" wrap="wrap">
                {Object.keys(Types).map((t) => (
                  <TypeSelector
                    name={t as TypeKey}
                    selected={types.has(t as TypeKey)}
                    setSelected={(selected) => {
                      if (selected) {
                        const newSet = new Set(types);
                        newSet.add(t as TypeKey);
                        setTypes(newSet);
                      } else {
                        const newSet = new Set(types);
                        newSet.delete(t as TypeKey);
                        setTypes(newSet);
                      }
                    }}
                  />
                ))}
              </Flex>
            </Flex>
          </Flex>
        </div>
      </Flex>
      <Button
        color="cyan"
        onClick={() =>
          props.doSearch(searchTerms, includeSequence, person, types)
        }
      >
        Do search
      </Button>
    </Flex>
  );
}

export default function (props: Props) {
  return (
    <Popover.Root>
      <Popover.Trigger>
        <IconButton variant="ghost" radius="none">
          <AccessibleIcon label="search">
            <MagnifyingGlassIcon width={20} height={20} />
          </AccessibleIcon>
        </IconButton>
      </Popover.Trigger>
      <Popover.Content style={{ width: 360 }}>
        <SearchPageContent doSearch={props.doSearch} />
      </Popover.Content>
    </Popover.Root>
  );
}

const styles = StyleSheet.create({
  filterDiv: {
    paddingLeft: 8,
    // Radix cyan-9
    borderLeft: `2px solid #00a2c7`,
  },
});
