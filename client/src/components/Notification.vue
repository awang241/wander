<template>
  <div class="card" :style="cardStyle">
    <div class="columns">
      <div class="column">
        <p style="white-space: pre-wrap" id="message">{{ notification.message }}</p>
        <p id="date">{{ dateFormat(notification.timeStamp) }}</p>
      </div>
      <b-button
          v-if="notification.activityId !== null"
          id="viewButton"
          class="is-pulled-right"
          @click="goToActivity(notification.activityId)">
        View Activity
      </b-button>
    </div>
  </div>
</template>

<script>
import toastMixin from "../mixins/toastMixin";
import store from "../store";
import router from "../router";
import dateTimeMixin from "../mixins/dateTimeMixin";

/**
 * Notification types that are classed as activity creations/additions and the associated colour green.
 * @type {{types: number[], colour: string}}
 */
const ACTIVITY_ADDITIONS = {
  types: ['ActivityCreated', 'ActivityOrganiserAdded', 'ActivityCreatorAdded', 'ActivityFollowerAdded', 'ActivityParticipantAdded', 'ParticipantCreated'],
  colour: "#99ff94"
};

/**
 * Notification types that are classed as removing/deleting activity/participation with associated colour red.
 * @type {{types: number[], colour: string}}
 */
const ACTIVITY_REMOVALS = {
  types: ['ActivityRemoved', 'ActivityOrganiserRemoved', 'ActivityFollowerRemoved', 'ActivityParticipantRemoved'],
  colour: "#f56e7a"
};


/**
 * Notification types that are classed as member changes in colour yellow.
 * @type {{types: number[], colour: string}}
 */
const ACTIVITY_CHANGES = {
  types: ['ActivityEdited', 'ActivityPrivacyChanged', 'ParticipationEdited'],
  colour: "#faf689"
};


/**
 * The colour for notifications caused by the currently logged in user.
 * @type {string}
 */
const OWN_CHANGE_COLOUR = "#ffffb0";

/**
 * The default notification colour.
 * @type {string}
 */
const DEFAULT_COLOUR = "#ffffff";

Object.freeze(ACTIVITY_ADDITIONS);
Object.freeze(ACTIVITY_REMOVALS);
Object.freeze(ACTIVITY_CHANGES);


export default {
  name: "Notification",
  mixins: [toastMixin, dateTimeMixin],
  data() {
    return {
      store: store,
      cardStyle: {
        backgroundColor: null
      }
    }
  },
  props: {
    notification: {
      type: Object,
      required: true
    }
  },
  /**
   * Sets the notification colour based on the notification type.
   */
  mounted() {
    if (this.isOwnNotification) {
      this.cardStyle.backgroundColor = OWN_CHANGE_COLOUR;
    } else if (ACTIVITY_ADDITIONS.types.includes(this.$props.notification.notificationType)) {
      this.cardStyle.backgroundColor = ACTIVITY_ADDITIONS.colour;
    } else if (ACTIVITY_REMOVALS.types.includes(this.$props.notification.notificationType)) {
      this.cardStyle.backgroundColor = ACTIVITY_REMOVALS.colour;
    } else if (ACTIVITY_CHANGES.types.includes(this.$props.notification.notificationType)) {
      this.cardStyle.backgroundColor = ACTIVITY_CHANGES.colour;
    } else {
      this.cardStyle.backgroundColor = DEFAULT_COLOUR
    }
  },
  methods: {
    goToActivity(id) {
      router.push({path: 'Activities/' + id})
    },
  },
  computed: {
    /**
     * Returns true if the notification was created by the currently logged in user; false otherwise
     */
    isOwnNotification() {
      return this.$props.notification.profileId === this.$store.getters.getUserId
    }
  }
}
</script>

<style scoped>
.card {
  padding: 1rem;
}

.is-vertical-center {
  display: flex;
  align-items: center;
  padding-right: 1rem;
}

#viewButton {
  margin: 0.5rem;
}
</style>

