<template>
  <div>
    <b-field label="Activity types">
      <b-taginput
          v-model="chosenActivityTypes"
          :data="filteredActivityTypes"
          autocomplete
          @typing="getFilteredActivityTypes"
          :open-on-focus="false"
          placeholder="Add an activity type">
      </b-taginput>
    </b-field>

    <div>
      <b-radio v-model="activitySearchType"
               name="all types"
               native-value="all">
        Matching all types
      </b-radio>
      <b-radio v-model="activitySearchType"
               name="any types"
               native-value="any">
        Matching any type
      </b-radio>
    </div>
  </div>
</template>

<script>

import Api from "../Api";

export default {
  name: "ActivityTypesField",
  data() {
    return {
      possibleActivityTypes: null,
      filteredActivityTypes: this.possibleActivityTypes,

    }
  },
  props: {
    chosenActivityTypes: {
      type: Array
    },
    activitySearchType: {
      type: String
    }
  },
  watch: {
    activitySearchType: function (newSearchMethod) {
      this.$emit("updateSearchMethod", newSearchMethod)
    },
    chosenActivityTypes: function (newActivityTypes) {
      this.$emit("updateChosenActivityTypes", newActivityTypes)
    }
  },
  methods: {
    getPossibleActivityTypes() {
      Api.getActivityTypesList()
          .then(response => this.possibleActivityTypes = response.data.allActivityTypes)
          .catch(() => this.warningToast("Could not get activity type list, please refresh"))
    },
    //Autocomplete to display activity types that finish the word the user is typing
    getFilteredActivityTypes(text) {
      this.filteredActivityTypes = this.possibleActivityTypes.filter((option) => {
        return option
            .toString()
            .toLowerCase()
            .indexOf(text.toLowerCase()) >= 0
      })
    },
  },
  mounted() {
    this.getPossibleActivityTypes()
  },
}
</script>

<style scoped>

</style>